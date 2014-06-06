/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2014 Marco Costalba, Joona Kiiski, Tord Romstad

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <algorithm> // For std::count
#include <cassert>

#include "movegen.h"
#include "search.h"
#include "thread.h"
#include "ucioption.h"

using namespace Search;

ThreadPool Threads; // Global object

extern void check_time();

namespace {

 // start_routine() is the C function which is called when a new thread
 // is launched. It is a wrapper to the virtual function idle_loop().

 extern "C" { long start_routine(ThreadBase* th) { th->idle_loop(); return 0; } }


 // Helpers to launch a thread after creation and joining before delete. Must be
 // outside Thread c'tor and d'tor because the object will be fully initialized
 // when start_routine (and hence virtual idle_loop) is called and when joining.

 template<typename T> T* new_thread() {
   T* th = new T();
   thread_create(th->handle, start_routine, th); // Will go to sleep
   return th;
 }

 void delete_thread(ThreadBase* th) {
   th->exit = true; // Search must be already finished
   th->notify_one();
   thread_join(th->handle); // Wait for thread termination
   delete th;
 }

}


// notify_one() wakes up the thread when there is some work to do

void ThreadBase::notify_one() {

  mutex.lock();
  sleepCondition.notify_one();
  mutex.unlock();
}


// wait_for() set the thread to sleep until condition 'b' turns true

void ThreadBase::wait_for(volatile const bool& b) {

  mutex.lock();
  while (!b) sleepCondition.wait(mutex);
  mutex.unlock();
}


// Thread c'tor just inits data and does not launch any execution thread.
// Such a thread will only be started when c'tor returns.

Thread::Thread() /* : splitPoints() */ { // Value-initialization bug in MSVC

  searching = false;
  maxPly = 0;
  idx = Threads.size(); // Starts from 0
  activePosition = idx == 0 ? &RootPos : new Position();
}


void Thread::abort_slaves(StateInfo* sp, int ply) {

  uint64_t slaves = sp->slavesMask;

  while (slaves)
  {
      int t = pop_lsb(&slaves);
      Threads[t]->set_abort(ply);
  }
}


// TimerThread::idle_loop() is where the timer thread waits msec milliseconds
// and then calls check_time(). If msec is 0 thread sleeps until it's woken up.

void TimerThread::idle_loop() {

  while (!exit)
  {
      mutex.lock();

      if (!exit)
          sleepCondition.wait_for(mutex, run ? Resolution : INT_MAX);

      mutex.unlock();

      if (run)
          check_time();
  }
}


// MainThread::idle_loop() is where the main thread is parked waiting to be started
// when there is a new search. The main thread will launch all the slave threads.

void MainThread::idle_loop() {

  while (true)
  {
      mutex.lock();

      thinking = false;

      while (!thinking && !exit)
      {
          Threads.sleepCondition.notify_one(); // Wake up the UI thread if needed
          sleepCondition.wait(mutex);
      }

      mutex.unlock();

      if (exit)
          return;

      searching = true;

      Search::think();

      assert(searching);

      searching = false;
  }
}


// init() is called at startup to create and launch requested threads, that will
// go immediately to sleep. We cannot use a c'tor because Threads is a static
// object and we need a fully initialized engine at this point due to allocation
// of Endgames in Thread c'tor.

void ThreadPool::init() {

  timer = new_thread<TimerThread>();
  push_back(new_thread<MainThread>());
  read_uci_options();
}


// exit() cleanly terminates the threads before the program exits. Cannot be done in
// d'tor because we have to terminate the threads before to free ThreadPool object.

void ThreadPool::exit() {

  delete_thread(timer); // As first because check_time() accesses threads data

  for (iterator it = begin(); it != end(); ++it)
      delete_thread(*it);
}


// read_uci_options() updates internal threads parameters from the corresponding
// UCI options and creates/destroys threads to match the requested number. Thread
// objects are dynamically allocated to avoid creating all possible threads
// in advance (which include pawns and material tables), even if only a few
// are to be used.

void ThreadPool::read_uci_options() {

  minimumSplitDepth = Options["Min Split Depth"] * ONE_PLY;
  size_t requested  = Options["Threads"];

  assert(requested > 0);

  // If zero (default) then set best minimum split depth automatically
  if (!minimumSplitDepth)
      minimumSplitDepth = requested < 8 ? 4 * ONE_PLY : 7 * ONE_PLY;

  while (size() < requested)
      push_back(new_thread<Thread>());

  while (size() > requested)
  {
      delete_thread(back());
      pop_back();
  }
}


// wait_for_think_finished() waits for main thread to go to sleep then returns

void ThreadPool::wait_for_think_finished() {

  MainThread* t = main();
  t->mutex.lock();
  while (t->thinking) sleepCondition.wait(t->mutex);
  t->mutex.unlock();
}


// start_thinking() wakes up the main thread sleeping in MainThread::idle_loop()
// so to start a new search, then returns immediately.

void ThreadPool::start_thinking(const Position& pos, const LimitsType& limits, StateStackPtr& states) {

  wait_for_think_finished();

  SearchTime = Time::now(); // As early as possible

  Signals.stopOnPonderhit = Signals.firstRootMove = false;
  Signals.stop = Signals.failedLowAtRoot = false;

  RootMoves.clear();
  RootPos = pos;
  Limits = limits;
  if (states.get()) // If we don't set a new position, preserve current state
  {
      SetupStates = states; // Ownership transfer here
      assert(!states.get());
  }

  for (MoveList<LEGAL> it(pos); *it; ++it)
      if (   limits.searchmoves.empty()
          || std::count(limits.searchmoves.begin(), limits.searchmoves.end(), *it))
          RootMoves.push_back(RootMove(*it));

  main()->thinking = true;
  main()->notify_one(); // Starts main thread
}
