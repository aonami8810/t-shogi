with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    code = f.read()

target = """      oneTempoWinBonus = if isMyKingSafe && (isCheck || isPressuringOppKing)
                         then 1200.0 -- 相手の攻めを受けきって敵玉を仕留める！
                         else 0.0
  in capVal + promoVal + checkVal + suicidePen + fourPieceBonus + fourPieceDefenseBonus + oneTempoWinBonus"""

replacement = """      oneTempoWinBonus = if isMyKingSafe && (isCheck || isPressuringOppKing)
                         then 1200.0 -- 相手の攻めを受けきって敵玉を仕留める！
                         else 0.0
                         
      -- 6. ★ 攻め駒が詰まされた場合の最大刺し違え（Desperado）
      -- 価値の低い駒（歩や香）との刺し違えを厳禁し、金銀角飛との刺し違えを最優先
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
                            in if cVal >= 500.0 then 3000.0 + cVal * 3.0 -- 金銀角飛との最大刺し違え！
                               else if cVal >= 300.0 then 500.0
                               else -8000.0 -- 歩との刺し違え（角歩交換・銀歩交換）の厳罰！
                          Nothing -> 0.0
              _ -> 0.0
          _ -> 0.0
  in capVal + promoVal + checkVal + suicidePen + fourPieceBonus + fourPieceDefenseBonus + oneTempoWinBonus + desperadoBonus"""

if target in code:
    code = code.replace(target, replacement, 1)
    with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
        f.write(code)
    print("Updated GenerateStratifiedCompleteBook.hs with desperadoBonus successfully!")
else:
    print("Could not find target string in GenerateStratifiedCompleteBook.hs!")
