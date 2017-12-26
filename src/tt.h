/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2017 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#ifndef TT_H_INCLUDED
#define TT_H_INCLUDED

#include "misc.h"
#include "types.h"

/// TTEntry struct is the 10 bytes transposition table entry, defined as below:
///
/// key        16 bit
/// move       16 bit
/// value      16 bit
/// eval value 16 bit
/// generation  6 bit
/// bound type  2 bit
/// depth       8 bit

struct TTEntry {

  Move  move()  const { return (Move )move16; }
  Value value() const { return (Value)value16; }
  Value eval()  const { return (Value)eval16; }
  Depth depth() const { return (Depth)(depth8 * int(ONE_PLY)); }
  Bound bound() const { return (Bound)(genBound8 & 0x3); }

  void save(Key k, Value v, Bound b, Depth d, Move m, Value ev, uint8_t g) {

    assert(d / ONE_PLY * ONE_PLY == d);

    // Preserve any existing move for the same position
    if (m || (uint16_t)k != key16)
        move16 = (uint16_t)m;

    // Don't overwrite more valuable entries
    if (  (uint16_t)k != key16
        || d / ONE_PLY > depth8 - 4
     /* || g != (genBound8 & 0xFC) // Matching non-zero keys are already refreshed by probe() */
        || b == BOUND_EXACT)
    {
        key16     = (uint16_t)k;
        value16   = (int16_t)v;
        eval16    = (int16_t)ev;
        genBound8 = (uint8_t)(g | b);
        depth8    = (int8_t)(d / ONE_PLY);
    }
  }

private:
  friend class TranspositionTable;

  uint16_t key16;
  uint16_t move16;
  int16_t  value16;
  int16_t  eval16;
  uint8_t  genBound8;
  int8_t   depth8;
};

/// A transposition table consists of a number of clusters and each cluster
/// consists of ClusterSize number of TTEntry. Each non-empty entry contains
/// information about exactly one position. To ensure best cache performance,
/// the size of a cluster should be a power of two and the transposition
/// table should be aligned on clusters.

class TranspositionTable {

  static const int CacheLineSize = 64;
  static const int ClusterSize = 3;

  struct Cluster {
    TTEntry entry[ClusterSize];
    char padding[2]; // Align to a divisor of the cache line size
  };

  static const int ClusterLog = 5; // One cluster is 2^5 = 32 bytes.

  static_assert(sizeof(Cluster) == 1 << ClusterLog, "Cluster size incorrect");

  // The TT size is a multiple of 1 MB, i.e. a multiple of 2^15 clusters.
  // We allow a maximum TT size of 1 TB, which is 2^35 clusters.
  // On 32-bit machines we select values that lead to efficient 32-bit code.
  static const int UnitLog = Is64Bit ? 15 : 0;
  static const int SizeLog = Is64Bit ? 35 : 32;

public:
 ~TranspositionTable() { free(mem); }
  void new_search() { generation8 += 4; } // Lower 2 bits are used by Bound
  uint8_t generation() const { return generation8; }
  TTEntry* probe(const Key key, bool& found) const;
  int hashfull() const;
  void resize(size_t mbSize);
  void clear();

  static const size_t MaxMB = Is64Bit ? sizeof(Cluster) << (SizeLog - 20) : 2048;

  // The 64-bit key value is scaled to get the cluster index. We give up
  // some precision to keep the result of the multiplication within 64 bits.
  TTEntry* first_entry(const Key key) const {
    return &table[((key >> (SizeLog - UnitLog)) * unitCount) >> (64 - SizeLog)].entry[0];
  }

private:
  size_t unitCount; // TT size in number of units.
  Cluster* table;
  void* mem;
  uint8_t generation8; // Size must be not bigger than TTEntry::genBound8
};

extern TranspositionTable TT;

#endif // #ifndef TT_H_INCLUDED
