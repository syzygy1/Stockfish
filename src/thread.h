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

#ifndef THREAD_H_INCLUDED
#define THREAD_H_INCLUDED

#include <bitset>
#include <vector>

#include "material.h"
#include "movepick.h"
#include "pawns.h"
#include "position.h"
#include "search.h"

const int MAX_THREADS = 128;
const int MAX_SPLITPOINTS_PER_THREAD = 8;

struct ConditionVariable {
  ConditionVariable() { cond_init(c); }
 ~ConditionVariable() { cond_destroy(c); }

  void wait(Mutex& m) { cond_wait(c, m.l); }
  void wait_for(Mutex& m, int ms) { timed_wait(c, m.l, ms); }
  void notify_one() { cond_signal(c); }

private:
  WaitCondition c;
};

struct Thread;


/// ThreadBase struct is the base of the hierarchy from where we derive all the
/// specialized thread classes.

struct ThreadBase {

  ThreadBase() : handle(NativeHandle()), exit(false) {}
  virtual ~ThreadBase() {}
  virtual void idle_loop() = 0;
  void notify_one();
  void wait_for(volatile const bool& b);

  Mutex mutex;
  ConditionVariable sleepCondition;
  NativeHandle handle;
  volatile bool exit;
};


/// Thread struct keeps together all the thread related stuff like locks, state
/// and especially split points. We also use per-thread pawn and material hash
/// tables so that once we get a pointer to an entry its life time is unlimited
/// and we don't have to care about someone changing the entry under our feet.

struct Thread : public ThreadBase {

  Thread();
  virtual void idle_loop();
  void help_slaves(int ply);
  template <bool> bool find_split_point();
  static void init_search_threads();
  bool aborted();
  int abort_value();
  void set_abort(int ply);
  void clear_abort(int ply);
  void set_splitpoint(int ply);
  void clear_splitpoint(int ply);
  static void abort_slaves(StateInfo* sp, int ply);
  static void clear_slave(StateInfo* sp, int idx);

  Material::Table materialTable;
  Endgames endgames;
  Pawns::Table pawnsTable;
  Position *activePosition;
  StateInfo *activeSplitPoint;
  Thread *masterThread;
  Search::Stack *searchStack;
  int basePly;
  size_t idx;
  int maxPly;
  volatile uint64_t splitPointMask;
  volatile int abort;
  volatile bool searching;
  volatile bool finished;
};

inline bool Thread::aborted() {
  return abort != 0;
}

inline int Thread::abort_value() {
  return abort;
}

inline void Thread::set_abort(int ply) {
  int tmp = 0;
  while (!__sync_val_compare_and_swap(&abort, &tmp, ply))
    if (tmp <= ply) break;
}

inline void Thread::clear_abort(int ply) {
  __sync_val_compare_and_swap(&abort, &ply, 0);
}

inline void Thread::set_splitpoint(int ply) {
  __sync_fetch_and_or(&splitPointMask, 1ULL << (ply - 1));
}

inline void Thread::clear_splitpoint(int ply) {
  __sync_fetch_and_and(&splitPointMask, ~(1ULL << (ply - 1)));
}

inline void Thread::clear_slave(StateInfo *sp, int idx) {
  __sync_fetch_and_xor(&sp->slavesMask, 1ULL << idx);
}


/// MainThread and TimerThread are derived classes used to characterize the two
/// special threads: the main one and the recurring timer.

struct MainThread : public Thread {
  MainThread() : thinking(true) {} // Avoid a race with start_thinking()
  virtual void idle_loop();
  volatile bool thinking;
};

struct TimerThread : public ThreadBase {
  TimerThread() : run(false) {}
  virtual void idle_loop();
  bool run;
  static const int Resolution = 5; // msec between two check_time() calls
};


/// ThreadPool struct handles all the threads related stuff like init, starting,
/// parking and, most importantly, launching a slave thread at a split point.
/// All the access to shared thread data is done through this class.

struct ThreadPool : public std::vector<Thread*> {

  void init(); // No c'tor and d'tor, threads rely on globals that should
  void exit(); // be initialized and are valid during the whole thread lifetime.

  MainThread* main() { return static_cast<MainThread*>((*this)[0]); }
  void read_uci_options();
  Thread* available_slave(const Thread* master) const;
  void wait_for_think_finished();
  void start_thinking(const Position&, const Search::LimitsType&, Search::StateStackPtr&);

  Depth minimumSplitDepth;
  Mutex mutex;
  ConditionVariable sleepCondition;
  TimerThread* timer;
};

extern ThreadPool Threads;

#endif // #ifndef THREAD_H_INCLUDED
