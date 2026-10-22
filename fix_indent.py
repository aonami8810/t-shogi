with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

new_lines_text = """                   majorThreatBonus =
                     let curBd = board st
                         threatenedMajors = [ (sq, pt) | (sq, Piece p pt) <- Map.toList curBd
                                                       , p == turn st, pt `elem` [Rook, PRook, Bishop, PBishop]
                                                       , any (\\(_, Piece _ opt) -> opt `elem` [Pawn, Lance, Knight, Silver]) (getPiecesCovering opp sq curBd) ]
                     in (case threatenedMajors of
                          [] -> 0.0
                          ((thSq, _):_) ->
                            case m of
                              NormalMove from to _ ->
                                if from == thSq
                                then
                                  let isStillThreatened = any (\\(_, Piece _ opt) -> opt `elem` [Pawn, Lance, Knight, Silver]) (getPiecesCovering opp to nextBd)
                                  in if not isStillThreatened then 3500.0 else 0.0
                                else -4500.0
                              DropMove _ _ -> -4500.0)
                   
                   -- 1手勝ち（速度勝ち: Speed Race Advantage）判定
                   -- 自玉が安全（王手・即詰みがない）状態で、敵玉に詰めろをかける、または1手差で迫る手筋
                   myKingSqNow = findKing (turn st) nextBd
                   isMyKingSafe = not (isKingUnderAttack (turn st) nextBd) &&
                                  (case myKingSqNow of
                                     Just (mkx, mky) ->
                                       let myNeighbors = [ (mkx + dx, mky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0), not (isOutOfBounds (mkx + dx, mky + dy)) ]
                                           oppPressures = length [ sq | sq <- myNeighbors, isCoveredBy opp sq nextBd ]
                                       in oppPressures <= 2
                                     Nothing -> False)
                   oppKingSqNext = findKing opp nextBd
                   isPressuringOppKing = case oppKingSqNext of
                     Just (okx, oky) -> abs (fst toSq - okx) <= 2 && abs (snd toSq - oky) <= 2
                     Nothing         -> False
                   oneTempoWinBonus = if isMyKingSafe && (isCheck || isPressuringOppKing)
                                      then 1200.0
                                      else 0.0
                   
                   -- 金銀桂香による鉄壁受け（4枚の攻めを受け止める合駒・引き受け・底歩・底香）
                   isDefendingMyKing = case myKingSqNow of
                     Just (mkx, mky) -> abs (fst toSq - mkx) <= 2 && abs (snd toSq - mky) <= 2 && ptMoved `elem` [Gold, Silver, Knight, Lance, Pawn]
                     Nothing         -> False
                   ironcladDefenseBonus = if isDefendingMyKing && not (null oppAttackers) && not (null friendlyDefenders)
                                          then 800.0
                                          else 0.0
                   
                   s = evaluateState weights (turn st) nextSt
"""

replacement = [l + '\n' for l in new_lines_text.split('\n')[:-1]]
lines[1666:1710] = replacement

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)
print('Fixed with actual file!')
