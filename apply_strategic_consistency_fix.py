with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

old_commitment = """-- | 2. 攻め筋の一貫性（Strategic Commitment）：戦型フラフラ乗り換え・振り直しの厳罰化
evalStrategicCommitment :: State -> Move -> Double
evalStrategicCommitment st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      hist = moveHistory st
      -- 先手の選択した主戦型の判定
      hasPlayedIshida = any (\\mvStr -> mvStr == "2h7h" || mvStr == "7f7e") hist
      hasPlayedNakabisha = any (\\mvStr -> mvStr == "2h5h" || mvStr == "5g5f") hist
      hasPlayedIbisha = any (\\mvStr -> mvStr == "2g2f" || mvStr == "2f2e") hist
  in case m of
       NormalMove from to _ ->
         case Map.lookup from curBd of
           Just (Piece p Rook) | p == plr ->
             let (fx, _) = from
                 (tx, _) = to
                 -- 振り直し（筋の転換）検知
                 isSwitchingMode =
                   if plr == Sente
                   then (hasPlayedIshida && tx `notElem` [7]) ||
                        (hasPlayedNakabisha && tx `notElem` [5]) ||
                        (hasPlayedIbisha && tx `notElem` [2])
                   else False
                 isPromo = False
                 isCapturing = Map.member to curBd
             in if isSwitchingMode && not isCapturing
                then -5000.0 -- 一度決めた主戦型を放棄して別の筋へ振り直す日和見を完全厳禁！
                else 0.0
           _ -> 0.0
       _ -> 0.0"""

new_commitment = """-- | 2. 攻め筋の一貫性（Strategic Commitment）：戦型フラフラ乗り換え・振り直し・分裂手の完全根絶
evalStrategicCommitment :: State -> Move -> Double
evalStrategicCommitment st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      hist = moveHistory st
      -- 先手の選択した主戦型の判定
      hasPlayedIshida = any (\\mvStr -> mvStr == "2h7h" || mvStr == "7f7e") hist
      hasPlayedNakabisha = any (\\mvStr -> mvStr == "2h5h" || mvStr == "5g5f") hist
      hasPlayedIbisha = any (\\mvStr -> mvStr == "2g2f" || mvStr == "2f2e") hist
  in case m of
       NormalMove from to _ ->
         let (fx, fy) = from
             (tx, ty) = to
             isCapturing = Map.member to curBd
         in case Map.lookup from curBd of
              -- 飛車の振り直し（筋の転換）厳罰
              Just (Piece p Rook) | p == plr ->
                let isSwitchingMode =
                      if plr == Sente
                      then (hasPlayedIshida && tx `notElem` [7]) ||
                           (hasPlayedNakabisha && tx `notElem` [5]) ||
                           (hasPlayedIbisha && tx `notElem` [2])
                      else False
                in if isSwitchingMode && not isCapturing
                   then -5000.0 -- 一度決めた主戦型を放棄して別の筋へ振り直す日和見を完全厳禁！
                   else 0.0

              -- 歩の突きによる戦型の分裂・不整合の厳罰
              Just (Piece p Pawn) | p == plr ->
                let isIbishaPawn = (plr == Sente && from == (2, 7) && to == (2, 6)) ||
                                   (plr == Gote && from == (8, 3) && to == (8, 4))
                    isIshidaPawn = (plr == Sente && from == (7, 6) && to == (7, 5)) ||
                                   (plr == Gote && from == (3, 4) && to == (3, 5))
                    -- 石田流・振飛車を選んだのに、居飛車の2筋の歩を突いて分裂する手
                    isContradictoryAdvance =
                      if plr == Sente
                      then ((hasPlayedIshida || hasPlayedNakabisha) && isIbishaPawn) ||
                           (hasPlayedIbisha && isIshidaPawn)
                      else False
                in if isContradictoryAdvance && not isCapturing
                   then -5000.0 -- 戦型方針と矛盾する分裂手を完全排除！
                   else 0.0
              _ -> 0.0
       _ -> 0.0"""

content = content.replace(old_commitment, new_commitment)

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("Applied strategic consistency fix successfully!")
