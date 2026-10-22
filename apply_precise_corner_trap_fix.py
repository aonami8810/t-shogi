with open('c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

# 1. Update isExposingCornerBishopBlunder
old_blunder = """          -- 7. 相手の手持ち角による「8八・2八（先手）/ 2二・8二（後手）への角打ち・香車取り馬作り」の隙を自ら生み出す大失着の完全排除
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
                isTrapExposed cornerSq lanceSq bd =
                  let isCornerEmpty = not (Map.member cornerSq bd)
                      isCornerUnguarded = not (hasSolidDefender cornerSq bd)
                      isLanceTarget = case Map.lookup lanceSq bd of
                        Just (Piece ppt pt) -> ppt == plr && pt == Lance
                        Nothing             -> False
                  in isCornerEmpty && isCornerUnguarded && isLanceTarget
                -- 元々安全だったマスに、今回の着手で新たに隙を作ってしまった
                createsNewTrap = any (\\(c, l) -> not (isTrapExposed c l bd) && isTrapExposed c l nextBd) targets
            in hasOppBishopInHand && createsNewTrap && not isCheck && not isCapturingValuable"""

assert old_blunder in text, "old_blunder not found"
text = text.replace(old_blunder, new_blunder, 1)
print("1. isExposingCornerBishopBlunder refined!")

# 2. Update antiCornerBishopDefenseScore
old_def = """        -- 4. ★ 8八・2八（先手）/ 2二・8二（後手）への角打ち・香車取り馬作り筋の事前完全防御
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
                             
            isTrapExposed cornerSq lanceSq bd =
              let isCornerEmpty = not (Map.member cornerSq bd)
                  isCornerUnguarded = not (hasSolidDefender cornerSq bd)
                  isLanceTarget = case Map.lookup lanceSq bd of
                    Just (Piece ppt pt) -> ppt == plr && pt == Lance
                    Nothing             -> False
              in isCornerEmpty && isCornerUnguarded && isLanceTarget
              
            evalTarget (cornerSq, lanceSq) =
              let wasExposed = isTrapExposed cornerSq lanceSq curBd
                  isNowExposed = isTrapExposed cornerSq lanceSq nextBd
              in if wasExposed && not isNowExposed
                 then 2500.0 -- 既存の隙を埋めて完璧に防衛した！
                 else if not wasExposed && isNowExposed
                 then -5000.0 -- 自ら隙を作り出した！
                 else 0.0
                 
            -- 飛車が敵の歩などの当たりを受けて安全に自陣（2八/8二）へ引く手（引飛車手筋）への加点
            rookRetreatBonus = case m of
              NormalMove from to _ ->
                case Map.lookup from curBd of
                  Just (Piece p Rook) | p == plr ->
                    let isUnderAttack = isSquareUnderAttack opp from curBd
                        isRetreatToHome = if plr == Sente then to == (2, 8) else to == (8, 2)
                    in if isUnderAttack && isRetreatToHome then 1200.0
                       else if isRetreatToHome then 600.0
                       else 0.0
                  _ -> 0.0
              _ -> 0.0
        in if hasOppBishopInHand
           then sum (map evalTarget targets) + rookRetreatBonus
           else rookRetreatBonus"""

assert old_def in text, "old_def not found"
text = text.replace(old_def, new_def, 1)
print("2. antiCornerBishopDefenseScore refined with cured-vs-created distinction and rook retreat bonus!")

with open('c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(text)

print("SUCCESS: Logic applied to ShogiUsiMorse-v4.hs!")
