with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

old_quiescence = """    isTacticalMove s m =
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

new_quiescence = """    isTacticalMove s m =
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
                 -- ★ 飛車先直射を遮断する歩打ち受け（P*8g 先手8七歩打 / P*2c 後手2三歩打）は最重要戦術手！
                 isRookInterceptionPawn =
                   pt == Pawn &&
                   ((plr == Sente && to == (8, 7) && (Map.lookup (8, 6) (board s) == Just (Piece opp Rook) || Map.lookup (8, 5) (board s) == Just (Piece opp Rook))) ||
                    (plr == Gote  && to == (2, 3) && (Map.lookup (2, 4) (board s) == Just (Piece opp Rook) || Map.lookup (2, 5) (board s) == Just (Piece opp Rook))))
             in (not isSuicideDrop && isCheck) || isRookInterceptionPawn"""

content = content.replace(old_quiescence, new_quiescence)

# Update scoreTacticalMove
old_tactical_score = """                 scoreTacticalMove m = case m of
                   NormalMove _ to isP ->
                     let victimVal = case Map.lookup to (board st) of
                           Just (Piece _ pt) -> pieceValue pt
                           Nothing           -> 0.0
                         promoVal = if isP then 200.0 else 0.0
                     in victimVal * 10.0 + promoVal
                   DropMove pt _ ->
                     if pt `elem` [Rook, Bishop] then -500.0 else 100.0"""

new_tactical_score = """                 scoreTacticalMove m = case m of
                   NormalMove _ to isP ->
                     let victimVal = case Map.lookup to (board st) of
                           Just (Piece _ pt) -> pieceValue pt
                           Nothing           -> 0.0
                         promoVal = if isP then 200.0 else 0.0
                     in victimVal * 10.0 + promoVal
                   DropMove Pawn to ->
                     if (plr == Sente && to == (8, 7)) || (plr == Gote && to == (2, 3))
                     then 8000.0 -- 飛車先遮断の歩打ち受けを最優先探索！
                     else 100.0
                   DropMove pt _ ->
                     if pt `elem` [Rook, Bishop] then -500.0 else 100.0"""

content = content.replace(old_tactical_score, new_tactical_score)

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("Applied quiescence interception fix successfully!")
