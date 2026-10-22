with open('c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

idx = text.find("defensiveBonus = evalDefensiveTechnique st m")
assert idx != -1, "defensiveBonus not found"

idx_s = text.find("s = evaluateState weights (turn st) nextSt", idx)
assert idx_s != -1, "s = evaluateState not found"

new_middle = """defensiveBonus = evalDefensiveTechnique st m
                    
                    -- ★ 大駒が敵の歩・香・桂・銀などの当たりに晒されている時の安全退避（引飛車等）ボーナス & 当たり放置大減点
                    majorThreatBonus =
                      let curBd = board st
                          threatenedMajors = [ (sq, pt) | (sq, Piece p pt) <- Map.toList curBd
                                                        , p == turn st, pt `elem` [Rook, PRook, Bishop, PBishop]
                                                        , any (\\(_, Piece _ opt) -> opt `elem` [Pawn, Lance, Knight, Silver]) (getPiecesCovering opp sq curBd) ]
                      in case threatenedMajors of
                           [] -> 0.0
                           ((thSq, _):_) ->
                             case m of
                               NormalMove from to _ ->
                                 if from == thSq
                                 then
                                   let isStillThreatened = any (\\(_, Piece _ opt) -> opt `elem` [Pawn, Lance, Knight, Silver]) (getPiecesCovering opp to nextBd)
                                   in if not isStillThreatened then 3500.0 else 0.0
                                 else -4500.0
                               DropMove _ _ -> -4500.0
                    
                    """

text = text[:idx] + new_middle + text[idx_s:]

# Add to sum
idx_sum = text.find("+ antiQuotientSafetyBonus + shufflingPen + defensiveBonus + evalThicknessAndDominance st m")
assert idx_sum != -1, "idx_sum not found"
orig_str = "+ antiQuotientSafetyBonus + shufflingPen + defensiveBonus + evalThicknessAndDominance st m"
new_str = orig_str + " + majorThreatBonus"
text = text[:idx_sum] + new_str + text[idx_sum + len(orig_str):]

with open('c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(text)

print("SUCCESS: majorThreatBonus inserted!")
