### Overview

Stockfish is a free UCI chess engine derived from Glaurung 2.1. It is
not a complete chess program and requires some UCI-compatible GUI
(e.g. XBoard with PolyGlot, eboard, Arena, Sigma Chess, Shredder, Chess
Partner or Fritz) in order to be used comfortably. Read the
documentation for your GUI of choice for information about how to use
Stockfish with it.

This version of Stockfish supports up to 64 CPUs. The engine defaults
to one search thread, so it is therefore recommended to inspect the value of
the *Threads* UCI parameter, and to make sure it equals the number of CPU
cores on your computer.

This version of Stockfish has support for Syzygybases.


### Files

This distribution of Stockfish consists of the following files:

  * Readme.md, the file you are currently reading.

  * Copying.txt, a text file containing the GNU General Public License.

  * src, a subdirectory containing the full source code, including a Makefile
    that can be used to compile Stockfish on Unix-like systems. For further
    information about how to compile Stockfish yourself read section below.

  * polyglot.ini, for using Stockfish with Fabien Letouzey's PolyGlot
    adapter.


### Syzygybases

**Configuration**

Syzygybases are configured using the UCI options "SyzygyProbeLimit" and
"SyzygyPath".

The option "SyzygyPath" should be set to the directory or directories
where the .rtbw and .rtbz files can be found. Multiple directories should
be separated by ";" on Windows and by ":" on Unix-based operating systems.

Example: `C:\tablebases\wdl345;C:\tablebases\wdl6;D:\tablebases\dtz345;D:\tablebases\dtz6`

It is recommended to store .rtbw files on an SSD. There is no loss in
storing the .rtbz files on a regular HDD.

**Note:** At the moment, the "SyzygyPath" option can only be set once. If you want to change it, you need to restart the engine.

If you have the 6-piece tables, set the value of "SyzygyProbeLimit" to 6 (the default).
If you only have the 5-piece table, set it to 5. Set the value of this option
to 0 if you want to temporarily disable tablebase probing.

**What to expect**  
If the engine is searching a position that is not in the tablebases (e.g.
a position with 7 pieces), it will access the tablebases during the search.
If the engine reports a large mate score, this means that it has found a
winning line into a tablebase position. Example: mate in 60 means 10 moves
into a winning tablebase position.

If the engine is given a position to search that is in the tablebases, it
will use the tablebases at the beginning of the search to preselect all
good moves, i.e. all moves that preserve the win or preserve the draw while
taking into account the 50-move rule.
It will then perform a search only on those moves. **The engine will not move
immediately**, unless there is only a single good move. **The engine might 
not report a mate score even when the position is won.** Instead, it reports
the score that is returned by the search.

It is therefore clear that behaviour is not identical to what one might
be used to with Nalimov tablebases. There are technical reasons for this
difference, the main technical reason being that Nalimov tablebases use the
DTM metric (distance-to-mate), while Syzygybases use a variation of the
DTZ metric (distance-to-zero, zero meaning any move that resets the 50-move
counter). This special metric is one of the reasons that Syzygybases are
more compact than Nalimov tablebases, while still storing all information
needed for optimal play and in addition being able to take into account
the 50-move rule.

In the near future an option will be added to switch between the current
behaviour and a mode in which Stockfish will immediately play one of the
good moves. This new mode will have the problem that it leads to unnatural
play once the engine has reached a tablebase position. For example, the
engine will prefer any winning pawn move (even those that lose material
and complicate the win) over moves that lead to a quick mate but have a
higher "distance-to-zero" value.


### Opening books

This version of Stockfish has support for PolyGlot opening books. For
information about how to create such books, consult the PolyGlot
documentation. The book file can be selected by setting the *Book File*
UCI parameter.


### Compiling it yourself

On Unix-like systems, it should be possible to compile Stockfish
directly from the source code with the included Makefile.

Stockfish has support for 32 or 64-bit CPUs, the hardware POPCNT
instruction, big-endian machines such as Power PC, and other platforms.

In general it is recommended to run `make help` to see a list of make
targets with corresponding descriptions. When not using the Makefile to
compile (for instance with Microsoft MSVC) you need to manually
set/unset some switches in the compiler command line; see file *types.h*
for a quick reference.


### Terms of use

Stockfish is free, and distributed under the **GNU General Public License**
(GPL). Essentially, this means that you are free to do almost exactly
what you want with the program, including distributing it among your
friends, making it available for download from your web site, selling
it (either by itself or as part of some bigger software package), or
using it as the starting point for a software project of your own.

The only real limitation is that whenever you distribute Stockfish in
some way, you must always include the full source code, or a pointer
to where the source code can be found. If you make any changes to the
source code, these changes must also be made available under the GPL.

For full details, read the copy of the GPL found in the file named
*Copying.txt*
