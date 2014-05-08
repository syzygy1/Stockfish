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

#include <cassert>

#include "movegen.h"
#include "position.h"

namespace {

  template<CastlingRight Cr, bool Checks, bool Chess960>
  ExtMove* generate_castling(const Position& pos, ExtMove* mlist, Color us) {

    static const bool KingSide = (Cr == WHITE_OO || Cr == BLACK_OO);

    if (pos.castling_impeded(Cr) || !pos.can_castle(Cr))
        return mlist;

    // After castling, the rook and king final positions are the same in Chess960
    // as they would be in standard chess.
    Square kfrom = pos.king_square(us);
    Square rfrom = pos.castling_rook_square(Cr);
    Square kto = relative_square(us, KingSide ? SQ_G1 : SQ_C1);
    Bitboard enemies = pos.pieces(~us);

    assert(!pos.checkers());

    const Square K = Chess960 ? kto > kfrom ? DELTA_W : DELTA_E
                              : KingSide    ? DELTA_W : DELTA_E;

    for (Square s = kto; s != kfrom; s += K)
        if (pos.attackers_to(s) & enemies)
            return mlist;

    // Because we generate only legal castling moves we need to verify that
    // when moving the castling rook we do not discover some hidden checker.
    // For instance an enemy queen in SQ_A1 when castling rook is in SQ_B1.
    if (Chess960 && (attacks_bb<ROOK>(kto, pos.pieces() ^ rfrom) & pos.pieces(~us, ROOK, QUEEN)))
        return mlist;

    Move m = make<CASTLING>(kfrom, rfrom);

    if (Checks && !pos.gives_check(m))
        return mlist;

    (mlist++)->move = m;

    return mlist;
  }


  template<GenType Type, Square Delta>
  inline ExtMove* generate_promotions(ExtMove* mlist, Bitboard pawnsOn7,
                                      Bitboard target, const CheckInfo* ci) {

    Bitboard b = shift_bb<Delta>(pawnsOn7) & target;

    while (b)
    {
        Square to = pop_lsb(&b);

        if (Type == CAPTURES || Type == EVASIONS || Type == NON_EVASIONS)
            (mlist++)->move = make<PROMOTION>(to - Delta, to, QUEEN);

        if (Type == QUIETS || Type == EVASIONS || Type == NON_EVASIONS)
        {
            (mlist++)->move = make<PROMOTION>(to - Delta, to, ROOK);
            (mlist++)->move = make<PROMOTION>(to - Delta, to, BISHOP);
            (mlist++)->move = make<PROMOTION>(to - Delta, to, KNIGHT);
        }

        // Knight promotion is the only promotion that can give a direct check
        // that's not already included in the queen promotion.
        if (Type == QUIET_CHECKS && (StepAttacksBB[W_KNIGHT][to] & ci->ksq))
            (mlist++)->move = make<PROMOTION>(to - Delta, to, KNIGHT);
        else
            (void)ci; // Silence a warning under MSVC
    }

    return mlist;
  }


  template<Color Us, GenType Type>
  ExtMove* generate_pawn_moves(const Position& pos, ExtMove* mlist,
                               Bitboard target, const CheckInfo* ci) {

    // Compute our parametrized parameters at compile time, named according to
    // the point of view of white side.
    const Color    Them     = (Us == WHITE ? BLACK    : WHITE);
    const Bitboard TRank8BB = (Us == WHITE ? Rank8BB  : Rank1BB);
    const Bitboard TRank7BB = (Us == WHITE ? Rank7BB  : Rank2BB);
    const Bitboard TRank3BB = (Us == WHITE ? Rank3BB  : Rank6BB);
    const Square   Up       = (Us == WHITE ? DELTA_N  : DELTA_S);
    const Square   Right    = (Us == WHITE ? DELTA_NE : DELTA_SW);
    const Square   Left     = (Us == WHITE ? DELTA_NW : DELTA_SE);

    Bitboard b1, b2, dc1, dc2, emptySquares;

    Bitboard pawnsOn7    = pos.pieces(Us, PAWN) & ~ci->pinned &  TRank7BB;
    Bitboard pawnsNotOn7 = pos.pieces(Us, PAWN) & ~ci->pinned & ~TRank7BB;

    Bitboard enemies = (Type == EVASIONS ? pos.pieces(Them) & target:
                        Type == CAPTURES ? target : pos.pieces(Them));

    // Single and double pawn pushes, no promotions
    if (Type != CAPTURES)
    {
        emptySquares = (Type == QUIETS || Type == QUIET_CHECKS ? target : ~pos.pieces());

	b1 = pawnsNotOn7;

        // Add in pinned pawns moving between the pinner and the king
	if (Type != EVASIONS && ci->pinned)
	  b1 |= pos.pieces(Us, PAWN) & ci->pinned & file_bb(pos.king_square(Us));

        b1 = shift_bb<Up>(b1)            & emptySquares;
        b2 = shift_bb<Up>(b1 & TRank3BB) & emptySquares;

        if (Type == EVASIONS) // Consider only blocking squares
        {
            b1 &= target;
            b2 &= target;
        }

        if (Type == QUIET_CHECKS)
        {
            b1 &= pos.attacks_from<PAWN>(ci->ksq, Them);
            b2 &= pos.attacks_from<PAWN>(ci->ksq, Them);

            // Add pawn pushes which give discovered check. This is possible only
            // if the pawn is not on the same file as the enemy king, because we
            // don't generate captures. Note that a possible discovery check
            // promotion has been already generated amongst the captures.
            if (pawnsNotOn7 & ci->dcCandidates)
            {
                dc1 = shift_bb<Up>(pawnsNotOn7 & ci->dcCandidates) & emptySquares & ~file_bb(ci->ksq);
                dc2 = shift_bb<Up>(dc1 & TRank3BB) & emptySquares;

                b1 |= dc1;
                b2 |= dc2;
            }
        }

        while (b1)
        {
            Square to = pop_lsb(&b1);
            (mlist++)->move = make_move(to - Up, to);
        }

        while (b2)
        {
            Square to = pop_lsb(&b2);
            (mlist++)->move = make_move(to - Up - Up, to);
        }
    }

    // Promotions and underpromotions
    if (pawnsOn7 && (Type != EVASIONS || (target & TRank8BB)))
    {
        if (Type == CAPTURES)
            emptySquares = ~pos.pieces();

        if (Type == EVASIONS)
            emptySquares &= target;

        mlist = generate_promotions<Type, Right>(mlist, pawnsOn7, enemies, ci);
        mlist = generate_promotions<Type, Left >(mlist, pawnsOn7, enemies, ci);
        mlist = generate_promotions<Type, Up>(mlist, pawnsOn7, emptySquares, ci);
    }

    // Standard and en-passant captures
    if (Type == CAPTURES || Type == EVASIONS || Type == NON_EVASIONS)
    {
        b1 = shift_bb<Right>(pawnsNotOn7) & enemies;
        b2 = shift_bb<Left >(pawnsNotOn7) & enemies;

        while (b1)
        {
            Square to = pop_lsb(&b1);
            (mlist++)->move = make_move(to - Right, to);
        }

        while (b2)
        {
            Square to = pop_lsb(&b2);
            (mlist++)->move = make_move(to - Left, to);
        }

        if (pos.ep_square() != SQ_NONE)
        {
            assert(rank_of(pos.ep_square()) == relative_rank(Us, RANK_6));

            // An en passant capture can be an evasion only if the checking piece
            // is the double pushed pawn and so is in the target. Otherwise this
            // is a discovery check and we are forced to do otherwise.
            if (Type == EVASIONS && !(target & (pos.ep_square() - Up)))
                return mlist;

            b1 = pos.pieces(Us, PAWN) & pos.attacks_from<PAWN>(pos.ep_square(), Them);
            assert(b1);

            b1 = pos.pieces(Us, PAWN) & pos.attacks_from<PAWN>(pos.ep_square(), Them);
            if (Type == EVASIONS)
                b1 &= ~ci->pinned;

            while (b1)
            {
                Square from = pop_lsb(&b1);

                if (Type != EVASIONS)
                {
                    Square ksq = pos.king_square(Us);

                    if ((ci->pinned & from) && !aligned(from, ksq, pos.ep_square()))
                        continue;
                    else
                    {
                        Bitboard occ = pos.pieces() ^ from ^ (pos.ep_square() - Up);
                        if (attacks_bb<ROOK>(ksq, occ) & pos.pieces(Them, ROOK, QUEEN) & rank_bb(ksq))
                            continue;
                    }
                }
                (mlist++)->move = make<ENPASSANT>(from, pos.ep_square());
            }
                
        }
    }

    // Captures by pinned pawns
    // FIXME: for QUIET_CHECKS do knight underprom caps?
    if (Type != EVASIONS && Type != QUIET_CHECKS && ci->pinned)
    {
        b1 = pos.pieces(Us, PAWN) & ci->pinned & PseudoAttacks[BISHOP][pos.king_square(Us)];
        if (Type == QUIETS)
            b1 &= TRank7BB;

        while (b1)
        {
            Square from = pop_lsb(&b1);

            b2 = pos.attacks_from<PAWN>(from, Us) & enemies & PseudoAttacks[BISHOP][pos.king_square(Us)];
            if (!b2) continue;

            Square to = lsb(b2);

            if (Type == QUIETS || (b2 & TRank8BB))
            {
                if (Type == CAPTURES || Type == EVASIONS || Type == NON_EVASIONS)
                    (mlist++)->move = make<PROMOTION>(from, to, QUEEN);

                if (Type == QUIETS || Type == EVASIONS || Type == NON_EVASIONS)
                {
                    (mlist++)->move = make<PROMOTION>(from, to, ROOK);
                    (mlist++)->move = make<PROMOTION>(from, to, BISHOP);
                    (mlist++)->move = make<PROMOTION>(from, to, KNIGHT);
                }
            }
            else
            {
                (mlist++)->move = make_move(from, to);
            }
        }
    }

    return mlist;
  }


  template<PieceType Pt, bool Checks> FORCE_INLINE
  ExtMove* generate_moves(const Position& pos, ExtMove* mlist, Color us,
                          Bitboard target, const CheckInfo* ci) {

    assert(Pt != KING && Pt != PAWN);

    Bitboard bb = pos.pieces(us, Pt) & ~ci->pinned;

    while (bb)
    {
        Square from = pop_lsb(&bb);

        if (Checks)
        {
            if (    (Pt == BISHOP || Pt == ROOK || Pt == QUEEN)
                && !(PseudoAttacks[Pt][from] & target & ci->checkSq[Pt]))
                continue;

            if (unlikely(ci->dcCandidates) && (ci->dcCandidates & from))
                continue;
        }

        Bitboard b = pos.attacks_from<Pt>(from) & target;

        if (Checks)
            b &= ci->checkSq[Pt];

        while (b)
            (mlist++)->move = make_move(from, pop_lsb(&b));
    }

    return mlist;
  }


  template<Color Us, GenType Type> FORCE_INLINE
  ExtMove* generate_all(const Position& pos, ExtMove* mlist, Bitboard target,
                        const CheckInfo* ci) {

    const bool Checks = Type == QUIET_CHECKS;

    mlist = generate_pawn_moves<Us, Type>(pos, mlist, target, ci);
    mlist = generate_moves<KNIGHT, Checks>(pos, mlist, Us, target, ci);
    mlist = generate_moves<BISHOP, Checks>(pos, mlist, Us, target, ci);
    mlist = generate_moves<  ROOK, Checks>(pos, mlist, Us, target, ci);
    mlist = generate_moves< QUEEN, Checks>(pos, mlist, Us, target, ci);

    // Deal with pinned pieces
    if (Type != EVASIONS && ci->pinned)
    {
	Bitboard bb;

        bb = pos.pieces(BISHOP, QUEEN) & ci->pinned & PseudoAttacks[BISHOP][pos.king_square(Us)];

        while (bb)
        {
            Square from = pop_lsb(&bb);
            Bitboard b = pos.attacks_from<BISHOP>(from) & target & PseudoAttacks[BISHOP][pos.king_square(Us)];
            while (b)
                (mlist++)->move = make_move(from, pop_lsb(&b));
        }

        bb = pos.pieces(ROOK, QUEEN) & ci->pinned & PseudoAttacks[ROOK][pos.king_square(Us)];

        while (bb)
        {
            Square from = pop_lsb(&bb);
            Bitboard b = pos.attacks_from<ROOK>(from) & target & PseudoAttacks[ROOK][pos.king_square(Us)];
            while (b)
                (mlist++)->move = make_move(from, pop_lsb(&b));
        }
    }

    if (Type != QUIET_CHECKS && Type != EVASIONS)
    {
        Square ksq = pos.king_square(Us);
        Bitboard b = pos.attacks_from<KING>(ksq) & target;
        while (b)
        {
            Square to = pop_lsb(&b);
            if (Type == QUIETS || !(pos.attackers_to(to) & pos.pieces(~Us)))
                (mlist++)->move = make_move(ksq, to);
        }
    }

    if (Type != CAPTURES && Type != EVASIONS && pos.can_castle(Us))
    {
        if (pos.is_chess960())
        {
            mlist = generate_castling<MakeCastling<Us,  KING_SIDE>::right, Checks, true>(pos, mlist, Us);
            mlist = generate_castling<MakeCastling<Us, QUEEN_SIDE>::right, Checks, true>(pos, mlist, Us);
        }
        else
        {
            mlist = generate_castling<MakeCastling<Us,  KING_SIDE>::right, Checks, false>(pos, mlist, Us);
            mlist = generate_castling<MakeCastling<Us, QUEEN_SIDE>::right, Checks, false>(pos, mlist, Us);
        }
    }

    return mlist;
  }


} // namespace


/// generate<CAPTURES> generates all pseudo-legal captures and queen
/// promotions. Returns a pointer to the end of the move list.
///
/// generate<QUIETS> generates all pseudo-legal non-captures and
/// underpromotions. Returns a pointer to the end of the move list.
///
/// generate<NON_EVASIONS> generates all pseudo-legal captures and
/// non-captures. Returns a pointer to the end of the move list.

template<GenType Type>
ExtMove* generate(const Position& pos, ExtMove* mlist) {

  assert(Type == CAPTURES || Type == QUIETS || Type == NON_EVASIONS);
  assert(!pos.checkers());

  Color us = pos.side_to_move();
  CheckInfo *ci = pos.check_info();

  Bitboard target = Type == CAPTURES     ?  pos.pieces(~us)
                  : Type == QUIETS       ? ~pos.pieces()
                  : Type == NON_EVASIONS ? ~pos.pieces(us) : 0;

  return us == WHITE ? generate_all<WHITE, Type>(pos, mlist, target, ci)
                     : generate_all<BLACK, Type>(pos, mlist, target, ci);
}

// Explicit template instantiations
template ExtMove* generate<CAPTURES>(const Position&, ExtMove*);
template ExtMove* generate<QUIETS>(const Position&, ExtMove*);
template ExtMove* generate<NON_EVASIONS>(const Position&, ExtMove*);


/// generate<QUIET_CHECKS> generates all pseudo-legal non-captures and knight
/// underpromotions that give check. Returns a pointer to the end of the move list.
template<>
ExtMove* generate<QUIET_CHECKS>(const Position& pos, ExtMove* mlist) {

  assert(!pos.checkers());

  Color us = pos.side_to_move();
  CheckInfo *ci = pos.check_info();
  Bitboard dc = ci->dcCandidates;

  while (dc)
  {
     Square from = pop_lsb(&dc);
     PieceType pt = type_of(pos.piece_on(from));

     if (pt == PAWN)
         continue; // Will be generated together with direct checks

     Bitboard b = pos.attacks_from(Piece(pt), from) & ~pos.pieces();

     if (pt == KING)
         b &= ~PseudoAttacks[QUEEN][ci->ksq];

     while (b)
         (mlist++)->move = make_move(from, pop_lsb(&b));
  }

  return us == WHITE ? generate_all<WHITE, QUIET_CHECKS>(pos, mlist, ~pos.pieces(), ci)
                     : generate_all<BLACK, QUIET_CHECKS>(pos, mlist, ~pos.pieces(), ci);
}


/// generate<EVASIONS> generates all pseudo-legal check evasions when the side
/// to move is in check. Returns a pointer to the end of the move list.
template<>
ExtMove* generate<EVASIONS>(const Position& pos, ExtMove* mlist) {

  assert(pos.checkers());

  Color us = pos.side_to_move();
  Square ksq = pos.king_square(us);
  Bitboard sliderAttacks = 0;
  Bitboard sliders = pos.checkers() & ~pos.pieces(KNIGHT, PAWN);

  // Find all the squares attacked by slider checkers. We will remove them from
  // the king evasions in order to skip known illegal moves, which avoids any
  // useless legality checks later on.
  while (sliders)
  {
      Square checksq = pop_lsb(&sliders);
      sliderAttacks |= LineBB[checksq][ksq] ^ checksq;
  }

  // Generate evasions for king, capture and non capture moves
  Bitboard b = pos.attacks_from<KING>(ksq) & ~pos.pieces(us) & ~sliderAttacks;
  while (b)
  {
      Square to = pop_lsb(&b);
      if (!(pos.attackers_to(to) & pos.pieces(~us)))
          (mlist++)->move = make_move(ksq, to);
  }

  if (more_than_one(pos.checkers()))
      return mlist; // Double check, only a king move can save the day

  // Generate blocking evasions or captures of the checking piece
  Square checksq = lsb(pos.checkers());
  Bitboard target = between_bb(checksq, ksq) | checksq;

  CheckInfo *ci = pos.check_info();
  return us == WHITE ? generate_all<WHITE, EVASIONS>(pos, mlist, target, ci)
                     : generate_all<BLACK, EVASIONS>(pos, mlist, target, ci);
}


/// generate<LEGAL> generates all the legal moves in the given position

template<>
ExtMove* generate<LEGAL>(const Position& pos, ExtMove* mlist) {

//  ExtMove *end, *cur = mlist;
//  Bitboard pinned = pos.pinned_pieces(pos.side_to_move());
//  Square ksq = pos.king_square(pos.side_to_move());

  return pos.checkers() ? generate<EVASIONS>(pos, mlist)
                        : generate<NON_EVASIONS>(pos, mlist);
#if 0
  while (cur != end)
      if (   (pinned || from_sq(cur->move) == ksq || type_of(cur->move) == ENPASSANT)
          && !pos.legal(cur->move, pinned))
          cur->move = (--end)->move;
      else
          ++cur;

  return end;
#endif
}
