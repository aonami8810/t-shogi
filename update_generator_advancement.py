with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

target = """      -- 7. 銀の前線進出・石田流本組のモース勾配流
      silverIshidaBonus =
        case m of
          NormalMove from to _ ->
            case Map.lookup from (board s) of
              Just (Piece p Silver) | p == plr ->
                let (fx, fy) = from
                    (tx, ty) = to
                    isAdv = if plr == Sente then ty < fy else ty > fy
                    isClimbing = if plr == Sente then (fx == 2 && tx == 2 && ty <= 6) || (fx `elem` [1,2,3] && tx `elem` [1,2] && ty <= 5)
                                 else (fx == 8 && tx == 8 && ty >= 4) || (fx `elem` [7,8,9] && tx `elem` [8,9] && ty >= 5)
                    isCentral = if plr == Sente then to `elem` [(5,6), (4,6), (6,6), (5,5), (4,5)]
                                else to `elem` [(5,4), (6,4), (4,4), (5,5), (6,5)]
                in if not isAdv then 0.0
                   else if isClimbing then 650.0
                   else if isCentral then 550.0
                   else 350.0
              Just (Piece p Rook) | p == plr ->
                let isSenteIshida = plr == Sente && ((to == (7, 8)) || (from == (7, 8) && to == (7, 6)))
                    isGoteIshida  = plr == Gote  && ((to == (3, 2)) || (from == (3, 2) && to == (3, 4)))
                    (fx, fy) = from
                    (tx, ty) = to
                    isWandering = not isSenteIshida && not isGoteIshida && fx /= tx && null (getPiecesCovering opp from (board s))
                in if isSenteIshida || isGoteIshida then 800.0
                   else if isWandering then -3500.0
                   else 0.0
              Just (Piece p Pawn) | p == plr ->
                let isSentePawn = plr == Sente && from == (7, 6) && to == (7, 5)
                    isGotePawn  = plr == Gote && from == (3, 4) && to == (3, 5)
                in if isSentePawn || isGotePawn then 700.0 else 0.0
              Just (Piece p Knight) | p == plr ->
                let isSenteKnight = plr == Sente && to == (7, 7) && Map.lookup (7, 6) (board s) == Just (Piece Sente Rook)
                    isGoteKnight  = plr == Gote && to == (3, 3) && Map.lookup (3, 4) (board s) == Just (Piece Gote Rook)
                in if isSenteKnight || isGoteKnight then 650.0 else 0.0
              _ -> 0.0
          _ -> 0.0
  in capVal + promoVal + checkVal + suicidePen + fourPieceBonus + fourPieceDefenseBonus + oneTempoWinBonus + desperadoBonus + silverIshidaBonus"""

replacement = """      -- 7. 銀の前線進出・石田流本組のモース勾配流および包括的前進ボーナス
      advancementBonus =
        case m of
          NormalMove from to _ ->
            let (fx, fy) = from
                (tx, ty) = to
                isAdv = if plr == Sente then ty < fy else ty > fy
            in case Map.lookup from (board s) of
              Just (Piece p Gold) | p == plr ->
                let isRetreat = if plr == Sente then ty > fy else ty < fy
                    isBottom = if plr == Sente then ty == 9 else ty == 1
                in if isRetreat then (if isBottom then -6000.0 else -4000.0) else 0.0
              Just (Piece p Silver) | p == plr ->
                let isClimbing = if plr == Sente then (fx `elem` [1,2,3] && tx `elem` [1,2,3] && ty <= 7)
                                 else (fx `elem` [7,8,9] && tx `elem` [7,8,9] && ty >= 3)
                    isCentral = if plr == Sente then to `elem` [(5,6), (4,6), (6,6), (5,5), (4,5)]
                                else to `elem` [(5,4), (6,4), (4,4), (5,5), (6,5)]
                in if not isAdv then 0.0
                   else if isClimbing then 1800.0
                   else if isCentral then 1800.0
                   else 800.0
              Just (Piece p Rook) | p == plr ->
                let isFloating = (plr == Sente && (to == (7,6) || to == (2,6) || to == (5,6))) ||
                                 (plr == Gote  && (to == (3,4) || to == (8,4) || to == (5,4)))
                    isLegitOpening = (plr == Sente && from == (2,8) && to `elem` [(7,8), (5,8), (6,8), (2,6)]) ||
                                     (plr == Gote  && from == (8,2) && to `elem` [(3,2), (5,2), (4,2), (8,4)])
                    isReswing = fx /= tx && not isLegitOpening
                in if isFloating then 3400.0
                   else if isLegitOpening then 2200.0
                   else if isReswing then -5000.0
                   else 0.0
              Just (Piece p Pawn) | p == plr ->
                let isRookPawn = if plr == Sente then fx == 2 && isAdv else fx == 8 && isAdv
                    isCenterPawn = fx == 5 && isAdv
                    isIshidaPawn = if plr == Sente then from == (7,6) && to == (7,5) else from == (3,4) && to == (3,5)
                in if isIshidaPawn then 1300.0
                   else if isRookPawn then (if to == (2,4) || to == (8,6) then 1500.0 else 1100.0)
                   else if isCenterPawn then (if to == (5,5) then 1300.0 else 1000.0)
                   else if isAdv then 350.0
                   else 0.0
              Just (Piece p Knight) | p == plr ->
                let isIshidaKnight = (plr == Sente && to == (7,7)) || (plr == Gote && to == (3,3))
                in if isIshidaKnight then 1200.0
                   else if isAdv then 600.0
                   else 0.0
              Just (Piece p Bishop) | p == plr ->
                if (plr == Sente && to == (9,7)) || (plr == Gote && to == (1,3)) then 900.0
                else if to `elem` [(7,7), (6,6), (5,7), (3,3), (4,4), (5,3)] then 800.0
                else 0.0
              _ -> 0.0
          _ -> 0.0
  in capVal + promoVal + checkVal + suicidePen + fourPieceBonus + fourPieceDefenseBonus + oneTempoWinBonus + desperadoBonus + advancementBonus"""

if target in content:
    content = content.replace(target, replacement)
    with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
        f.write(content)
    print("Successfully updated GenerateStratifiedCompleteBook.hs")
else:
    print("Target block not found in GenerateStratifiedCompleteBook.hs!")
