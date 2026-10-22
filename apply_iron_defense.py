# Python script to update ShogiUsiMorse-v4.hs with ironclad gold defense and anti-bishop kobin defense

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

# Normalize to LF
is_crlf = '\r\n' in content
if is_crlf:
    content = content.replace('\r\n', '\n')

# 1. Update wanderingGoldPen
old_wandering = """          wanderingGoldPen = case m of
            NormalMove from to _ ->
              case Map.lookup from curBd of
                Just (Piece p Gold) | p == plr ->
                  let curKing = findKing plr curBd
                      oppKing = findKing opp curBd
                      isCapturing = Map.member to curBd
                      isNearOppKing = case oppKing of
                        Just (ekx, eky) -> max (abs (fst to - ekx)) (abs (snd to - eky)) <= 2
                        Nothing         -> False
                  in if isCapturing || isNearOppKing
                     then 0.0
                     else
                       let distCur = case curKing of Just (kx, ky) -> abs (fst from - kx) + abs (snd from - ky); Nothing -> 0
                           distNext = case curKing of Just (kx, ky) -> abs (fst to - kx) + abs (snd to - ky); Nothing -> 0
                           movingAwayPen = if distNext > distCur && distNext > 2 then -600.0 else 0.0
                           isMidRank = snd to == 5 || (plr == Sente && snd to == 6) || (plr == Gote && snd to == 4)
                           midRankPen = if isMidRank then -500.0 else 0.0
                       in movingAwayPen + midRankPen
                _ -> 0.0
            _ -> 0.0"""

new_wandering = """          wanderingGoldPen = case m of
            NormalMove from to _ ->
              case Map.lookup from curBd of
                Just (Piece p Gold) | p == plr ->
                  let curKing = findKing plr curBd
                      oppKing = findKing opp curBd
                      isCapturing = Map.member to curBd
                      isNearOppKing = case oppKing of
                        Just (ekx, eky) -> max (abs (fst to - ekx)) (abs (snd to - eky)) <= 2
                        Nothing         -> False
                      isHomeSquare = if plr == Sente
                                     then to == (4, 9) || to == (6, 9)
                                     else to == (6, 1) || to == (4, 1)
                      homePen = if isHomeSquare then -5000.0 else 0.0
                  in if isCapturing || isNearOppKing
                     then 0.0
                     else
                       let distCur = case curKing of Just (kx, ky) -> abs (fst from - kx) + abs (snd from - ky); Nothing -> 0
                           distNext = case curKing of Just (kx, ky) -> abs (fst to - kx) + abs (snd to - ky); Nothing -> 0
                           movingAwayPen = if distNext > distCur && distNext > 2 then -600.0 else 0.0
                           isMidRank = snd to == 5 || (plr == Sente && snd to == 6) || (plr == Gote && snd to == 4)
                           midRankPen = if isMidRank then -500.0 else 0.0
                       in homePen + movingAwayPen + midRankPen
                _ -> 0.0
            _ -> 0.0"""

assert old_wandering in content, "old_wandering not found in content"
content = content.replace(old_wandering, new_wandering, 1)
print("Updated wanderingGoldPen successfully!")

# 2. Update solidDefenseScore and antiCornerBishopDefenseScore in evalDefensiveTechnique
old_defensive = """      -- 2. 金銀の引き受け・玉との連結（Gold/Silver Retreat & Solidification）
      solidDefenseScore = case m of
        NormalMove from to _ ->
          case Map.lookup from curBd of
            Just (Piece p pt) | p == plr && pt `elem` [Gold, Silver] ->
              let isRetreat = if plr == Sente then snd to > snd from else snd to < snd from
                  distNext = case nextKingSq of
                    Just (kx, ky) -> abs (fst to - kx) + abs (snd to - ky)
                    Nothing       -> 99
                  distCur = case curKingSq of
                    Just (kx, ky) -> abs (fst from - kx) + abs (snd from - ky)
                    Nothing       -> 99
                  isClosingToKing = distNext < distCur || (distNext <= 2 && distCur <= 2)
                  defendsFriendly = any (\\(sq, Piece own _) -> own == plr && isSquareUnderAttack opp sq curBd && isSquareUnderAttack plr sq nextBd) (Map.toList nextBd)
                  defendBonus = if defendsFriendly then 280.0 else 0.0
                  retreatBonus = if isRetreat && isClosingToKing then 320.0 else 0.0
              in retreatBonus + defendBonus
            _ -> 0.0
        _ -> 0.0
        
      -- 3. 自陣の角打ち隙（両取り隙）を消す受け
      antiForkDefenseScore =
        case curKingSq of
          Just (kx, ky) ->
            let toSq = case m of NormalMove _ t _ -> t; DropMove _ t -> t
                isDefendingRear = if plr == Sente then snd toSq >= 8 else snd toSq <= 2
            in if isDefendingRear then 180.0 else 0.0
          Nothing -> 0.0
          
        -- 4. ★ 8八・2八（先手）/ 2二・8二（後手）への角打ち・香車取り馬作り筋の事前完全防御
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
           else rookRetreatBonus
          
  in pawnDropDefenseScore + solidDefenseScore + antiForkDefenseScore + antiCornerBishopDefenseScore"""

new_defensive = """      -- 2. 金銀の引き受け・玉との連結（Gold/Silver Solidification）
      -- ★ 厳重注意：金が理由なく初期位置（先手4九・6九 / 後手6一・4一）に戻る手（Home Retreat Blunder）は、
      --   陣形破壊・コビン開放・角打ち被弾の主因であり、絶対にボーナスを与えず特大ペナルティを科す！
      solidDefenseScore = case m of
        NormalMove from to _ ->
          case Map.lookup from curBd of
            Just (Piece p pt) | p == plr && pt `elem` [Gold, Silver] ->
              let isRetreat = if plr == Sente then snd to > snd from else snd to < snd from
                  distNext = case nextKingSq of
                    Just (kx, ky) -> abs (fst to - kx) + abs (snd to - ky)
                    Nothing       -> 99
                  distCur = case curKingSq of
                    Just (kx, ky) -> abs (fst from - kx) + abs (snd from - ky)
                    Nothing       -> 99
                  isClosingToKing = distNext < distCur || (distNext <= 2 && distCur <= 2)
                  
                  -- 金の初期位置への戻り判定 (Home Retreat Blunder)
                  isGold = pt == Gold
                  isHomeSquare = if plr == Sente
                                 then to == (4, 9) || to == (6, 9)
                                 else to == (6, 1) || to == (4, 1)
                  isCapture = Map.member to curBd
                  inCheckNow = isKingUnderAttack plr curBd
                  
                  -- 中飛車陣形における要石（Keystone）の放棄判定
                  -- 5八, 4八, 6八, 5七, 4七, 6七 の金が1段目に後退する手を厳禁
                  isAbandoningCentralKeystone =
                    if plr == Sente
                    then isGold && snd from <= 8 && snd to == 9
                    else isGold && snd from >= 2 && snd to == 1
                    
                  homeRetreatPen =
                    if isGold && isHomeSquare && not isCapture && not inCheckNow
                    then -6000.0 -- 金が元の位置に戻る自滅手には特大ペナルティ！
                    else if isAbandoningCentralKeystone && not isCapture && not inCheckNow
                    then -5000.0 -- 要石の金を無意味に1段目に引く手損・陣形破壊ペナルティ！
                    else 0.0
                    
                  defendsFriendly = any (\\(sq, Piece own _) -> own == plr && isSquareUnderAttack opp sq curBd && isSquareUnderAttack plr sq nextBd) (Map.toList nextBd)
                  defendBonus = if defendsFriendly then 280.0 else 0.0
                  -- 初期位置への退行でない健全な引き受けのみボーナス
                  retreatBonus = if isRetreat && isClosingToKing && not isHomeSquare && not isAbandoningCentralKeystone
                                 then 320.0
                                 else 0.0
              in homeRetreatPen + retreatBonus + defendBonus
            _ -> 0.0
        _ -> 0.0
        
      -- 3. 自陣の角打ち隙（両取り隙）を消す受け
      antiForkDefenseScore =
        case curKingSq of
          Just (kx, ky) ->
            let toSq = case m of NormalMove _ t _ -> t; DropMove _ t -> t
                isDefendingRear = if plr == Sente then snd toSq >= 8 else snd toSq <= 2
            in if isDefendingRear then 180.0 else 0.0
          Nothing -> 0.0
          
      -- 4. ★ 自陣全域（特に中飛車のコビン・玉頭・飛車横）への角打ち・馬作り被弾の完全防止
      --    相手が持ち駒に角を持っている場合、自らの手によって角打ちの隙（コビン）を開放した場合は超厳罰（-8000点）！
      antiBishopDropAndKobinDefenseScore =
        let oppHand = Map.findWithDefault Map.empty opp (hands st)
            hasOppBishopInHand = Map.findWithDefault 0 Bishop oppHand > 0
            
            -- 自陣の重要マス（先手7..9段目、後手1..3段目）
            myCampY = if plr == Sente then [7, 8, 9] else [1, 2, 3]
            campSquares = [ (x, y) | x <- [1..9], y <- myCampY ]
            
            -- ある盤面において、相手が角を打てる危険マス（空きマスで、自軍の利きがなく、相手の馬作りや両取りが確定するマス）
            isDangerousBishopDrop bd sq =
              not (Map.member sq bd) &&
              let myDefenders = getPiecesCovering plr sq bd
                  isProtected = not (null myDefenders)
                  
                  isNearMyKing = case (if plr == Sente then nextKingSq else curKingSq) of
                    Just (kx, ky) -> max (abs (fst sq - kx)) (abs (snd sq - ky)) <= 2
                    Nothing       -> False
                    
                  isCentralKeystone = if plr == Sente
                                      then sq `elem` [(3,9), (4,9), (5,8), (6,9), (7,9), (4,8), (6,8), (5,7)]
                                      else sq `elem` [(7,1), (6,1), (5,2), (4,1), (3,1), (6,2), (4,2), (5,3)]
                                      
                  testBdWithBishop = Map.insert sq (Piece opp Bishop) bd
                  threatenedSquares = [ tsq | (tsq, Piece p pt) <- Map.toList testBdWithBishop
                                            , p == plr
                                            , pt `elem` [King, Rook, PRook, Gold, Silver]
                                            , canPieceReachTarget sq (Piece opp Bishop) tsq testBdWithBishop ]
                  isForkingHighValue = length threatenedSquares >= 2 || any (\\tsq -> Map.lookup tsq bd == Just (Piece plr King)) threatenedSquares
              in not isProtected && (isNearMyKing || isCentralKeystone || isForkingHighValue)

            curDangerousCount = length (filter (isDangerousBishopDrop curBd) campSquares)
            nextDangerousCount = length (filter (isDangerousBishopDrop nextBd) campSquares)
            
            kobinDiff = nextDangerousCount - curDangerousCount
            kobinScore =
              if kobinDiff > 0
              then -8000.0 * fromIntegral kobinDiff
              else if kobinDiff < 0
              then 2200.0 * fromIntegral (- kobinDiff)
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
           then kobinScore + rookRetreatBonus
           else rookRetreatBonus
          
  in pawnDropDefenseScore + solidDefenseScore + antiForkDefenseScore + antiBishopDropAndKobinDefenseScore"""

assert old_defensive in content, "old_defensive not found in content"
content = content.replace(old_defensive, new_defensive, 1)
print("Updated evalDefensiveTechnique successfully!")

if is_crlf:
    content = content.replace('\n', '\r\n')

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)
print("ShogiUsiMorse-v4.hs saved successfully!")
