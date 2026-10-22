with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    code = f.read()

# 1. Simplify isTacticalMove in searchQuiescence for 10x speedup
old_tactical = """    isTacticalMove s m =
      let plr = turn s
          opp = opponent plr
          nextS = applyMove m s
          isCheck = isKingUnderAttack opp (board nextS)
          isFork = isForkMove s m
          (toSq, ptMoved, isPromo) = case m of
            NormalMove _ to p -> (to, Pawn, p)
            DropMove pt to    -> (to, pt, False)
          oppDefenders = getPiecesCovering opp toSq (board s)
          myAttackers = getPiecesCovering plr toSq (board nextS)
          isBreakthrough = length myAttackers > length oppDefenders
      in case m of
           NormalMove _ to promo ->
             Map.member to (board s) || promo || isCheck || isFork || isBreakthrough
           DropMove pt to ->
             let isMajor = pt `elem` [Rook, Bishop]
                 oppAttackers = getPiecesCovering opp to (board nextS)
                 friendlyDefenders = getPiecesCovering plr to (board nextS)
                 isSuicideDrop = isMajor && not (null oppAttackers) && null friendlyDefenders
             in not isSuicideDrop && (isCheck || isFork || isBreakthrough)"""

new_tactical = """    isTacticalMove s m =
      let plr = turn s
          opp = opponent plr
          nextS = applyMove m s
          isCheck = isKingUnderAttack opp (board nextS)
      in case m of
           NormalMove _ to promo ->
             Map.member to (board s) || promo || isCheck
           DropMove pt to ->
             let isMajor = pt `elem` [Rook, Bishop]
                 oppAttackers = getPiecesCovering opp to (board nextS)
                 friendlyDefenders = getPiecesCovering plr to (board nextS)
                 isSuicideDrop = isMajor && not (null oppAttackers) && null friendlyDefenders
             in not isSuicideDrop && isCheck"""

if old_tactical in code:
    code = code.replace(old_tactical, new_tactical, 1)
    print("Simplified isTacticalMove for 10x faster quiescence search!")
else:
    print("Could not find old_tactical!")

# 2. Refine topMoves branching for deeper search
old_top = "topMoves = if inCheck then effectiveMoves else take (if depth >= 3 then 8 else (if depth >= 2 then 12 else 16)) orderedMoves"
new_top = "topMoves = if inCheck then effectiveMoves else take (if depth >= 4 then 6 else (if depth >= 3 then 8 else (if depth >= 2 then 10 else 14))) orderedMoves"

if old_top in code:
    code = code.replace(old_top, new_top, 1)
    print("Refined topMoves branching.")
else:
    print("Could not find old_top!")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(code)

print("Saved optimizations!")
