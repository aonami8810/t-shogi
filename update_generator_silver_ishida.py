with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

target = """      -- 6. 攻め駒が詰まされた場合の最大刺し違え（Desperado）
      desperadoBonus =
        case m of
          NormalMove from to _ ->
            case Map.lookup from (board s) of
              Just (Piece _ pt) | pt `elem` [Rook, PRook, Bishop, PBishop, Silver, PSilver, Knight] ->
                let isTrapped = not (null (getPiecesCovering opp from (board s)))
                in if not isTrapped then 0.0
                   else case targetCaptured of
                          Just (Piece _ capPt) ->
                            let cVal = pieceValue capPt
                            in if cVal >= 500.0 then 3000.0 + cVal * 3.0
                               else if cVal >= 300.0 then 500.0
                               else -8000.0
                          Nothing -> 0.0
              _ -> 0.0
          _ -> 0.0
  in capVal + promoVal + checkVal + suicidePen + fourPieceBonus + fourPieceDefenseBonus + oneTempoWinBonus + desperadoBonus"""

replacement = """      -- 6. 攻め駒が詰まされた場合の最大刺し違え（Desperado）
      desperadoBonus =
        case m of
          NormalMove from to _ ->
            case Map.lookup from (board s) of
              Just (Piece _ pt) | pt `elem` [Rook, PRook, Bishop, PBishop, Silver, PSilver, Knight] ->
                let isTrapped = not (null (getPiecesCovering opp from (board s)))
                in if not isTrapped then 0.0
                   else case targetCaptured of
                          Just (Piece _ capPt) ->
                            let cVal = pieceValue capPt
                            in if cVal >= 500.0 then 3000.0 + cVal * 3.0
                               else if cVal >= 300.0 then 500.0
                               else -8000.0
                          Nothing -> 0.0
              _ -> 0.0
          _ -> 0.0

      -- 7. 銀の前線進出・石田流本組のモース勾配流
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

if target in content:
    content = content.replace(target, replacement)
    with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
        f.write(content)
    print("Successfully updated GenerateStratifiedCompleteBook.hs")
else:
    print("Target block not found!")
