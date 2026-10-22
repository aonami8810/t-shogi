with open('c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

# 1. Update isExposingCornerBishopBlunder in isBlunderMove to check both 88 & 28 for Sente, and 22 & 82 for Gote
old_blunder = """          -- 7. 相手の手持ち角による「8八（先手）/ 2二（後手）への角打ち・香車取り馬作り」の隙を自ら生み出す大失着の完全排除
          isExposingCornerBishopBlunder =
            let oppHand = Map.findWithDefault Map.empty opp (hands st)
                hasOppBishopInHand = Map.findWithDefault 0 Bishop oppHand > 0
                (cornerSq, lanceSq) = if plr == Sente then ((8, 8), (9, 9)) else ((2, 2), (1, 1))
                hasSolidDefender bd =
                  not (null [ sq | (sq, Piece p pt) <- Map.toList bd
                                 , p == plr && pt `elem` [Gold, Silver]
                                 , canPieceReachTarget sq (Piece p pt) cornerSq bd ])
                isCornerEmpty = not (Map.member cornerSq nextBd)
                isCornerUnguarded = not (hasSolidDefender nextBd)
                isLanceTarget = case Map.lookup lanceSq nextBd of
                  Just (Piece ppt pt) -> ppt == plr && pt == Lance
                  Nothing             -> False
                isExposed = hasOppBishopInHand && isCornerEmpty && isCornerUnguarded && isLanceTarget
            in isExposed && not isCheck && not isCapturingValuable"""

new_blunder = """          -- 7. 相手の手持ち角による「8八・2八（先手）/ 2二・8二（後手）への角打ち・香車取り馬作り」の隙を自ら生み出す大失着の完全排除
          isExposingCornerBishopBlunder =
            let oppHand = Map.findWithDefault Map.empty opp (hands st)
                hasOppBishopInHand = Map.findWithDefault 0 Bishop oppHand > 0
                targets = if plr == Sente
                          then [ ((8, 8), (9, 9)), ((2, 8), (1, 9)) ]
                          else [ ((2, 2), (1, 1)), ((8, 2), (9, 1)) ]
                hasSolidDefender cornerSq bd =
                  not (null [ sq | (sq, Piece p pt) <- Map.toList bd
                                 , p == plr && pt `elem` [Gold, Silver, King, Rook]
                                 , canPieceReachTarget sq (Piece p pt) cornerSq bd ])
                isTrapExposed (cornerSq, lanceSq) =
                  let isCornerEmpty = not (Map.member cornerSq nextBd)
                      isCornerUnguarded = not (hasSolidDefender cornerSq nextBd)
                      isLanceTarget = case Map.lookup lanceSq nextBd of
                        Just (Piece ppt pt) -> ppt == plr && pt == Lance
                        Nothing             -> False
                  in isCornerEmpty && isCornerUnguarded && isLanceTarget
                anyExposed = hasOppBishopInHand && any isTrapExposed targets
            in anyExposed && not isCheck && not isCapturingValuable"""

assert old_blunder in text, "old_blunder not found"
text = text.replace(old_blunder, new_blunder, 1)
print("1. isExposingCornerBishopBlunder updated with 28 & 82 support!")

# 2. Update antiCornerBishopDefenseScore in evalDefensiveTechnique
old_def = """        -- 4. ★ 8八（先手）/ 2二（後手）への角打ち・香車取り馬作り筋の事前完全防御
      antiCornerBishopDefenseScore =
        let oppHand = Map.findWithDefault Map.empty opp (hands st)
            hasOppBishopInHand = Map.findWithDefault 0 Bishop oppHand > 0
            (cornerSq, lanceSq) = if plr == Sente then ((8, 8), (9, 9)) else ((2, 2), (1, 1))
            
            -- 金・銀による近接直接防衛（飛車の遠隔利きに依存しない鉄壁防衛）
            hasSolidDefender bd =
              not (null [ sq | (sq, Piece p pt) <- Map.toList bd
                             , p == plr && pt `elem` [Gold, Silver]
                             , canPieceReachTarget sq (Piece p pt) cornerSq bd ])
                             
            isCornerOccupied = Map.member cornerSq nextBd
            isCornerSafelyGuarded = hasSolidDefender nextBd
            
            wasCornerEmpty = not (Map.member cornerSq curBd)
            wasCornerUnguarded = not (hasSolidDefender curBd)
            hadExposedLance = case Map.lookup lanceSq curBd of
              Just (Piece ppt pt) -> ppt == plr && pt == Lance
              Nothing             -> False
            wasExposed = hasOppBishopInHand && wasCornerEmpty && wasCornerUnguarded && hadExposedLance
            
            -- 今回の手によって隙を埋めた（8八銀、7八金、7七銀、角道止め等）
            curedTheTrap = wasExposed && (isCornerOccupied || isCornerSafelyGuarded)
            
            -- 逆に隙を放置して無関係な手を指した減点
            leftExposed = wasExposed && not isCornerOccupied && not isCornerSafelyGuarded
        in if curedTheTrap then 1500.0
           else if leftExposed then -3000.0
           else 0.0"""

new_def = """        -- 4. ★ 8八・2八（先手）/ 2二・8二（後手）への角打ち・香車取り馬作り筋の事前完全防御
      antiCornerBishopDefenseScore =
        let oppHand = Map.findWithDefault Map.empty opp (hands st)
            hasOppBishopInHand = Map.findWithDefault 0 Bishop oppHand > 0
            targets = if plr == Sente
                      then [ ((8, 8), (9, 9)), ((2, 8), (1, 9)) ]
                      else [ ((2, 2), (1, 1)), ((8, 2), (9, 1)) ]
                      
            hasSolidDefender cornerSq bd =
              not (null [ sq | (sq, Piece p pt) <- Map.toList bd
                             , p == plr && pt `elem` [Gold, Silver, King, Rook]
                             , canPieceReachTarget sq (Piece p pt) cornerSq bd ])
                             
            evalTarget (cornerSq, lanceSq) =
              let isCornerOccupied = Map.member cornerSq nextBd
                  isCornerSafelyGuarded = hasSolidDefender cornerSq nextBd
                  wasCornerEmpty = not (Map.member cornerSq curBd)
                  wasCornerUnguarded = not (hasSolidDefender cornerSq curBd)
                  hadExposedLance = case Map.lookup lanceSq curBd of
                    Just (Piece ppt pt) -> ppt == plr && pt == Lance
                    Nothing             -> False
                  wasExposed = hasOppBishopInHand && wasCornerEmpty && wasCornerUnguarded && hadExposedLance
                  cured = wasExposed && (isCornerOccupied || isCornerSafelyGuarded)
                  left = wasExposed && not isCornerOccupied && not isCornerSafelyGuarded
              in if cured then 1500.0
                 else if left then -3000.0
                 else 0.0
        in sum (map evalTarget targets)"""

assert old_def in text, "old_def not found"
text = text.replace(old_def, new_def, 1)
print("2. antiCornerBishopDefenseScore updated with 28 & 82 support!")

with open('c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(text)

print("SUCCESS: 28 & 82 Anti-Bishop-Trap logic applied!")
