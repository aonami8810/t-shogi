with open('c:\\VS\\Workspace\\t-shogi\\ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

# 1. Add evalPieceShuffling and evalDefensiveTechnique right after evalAntiQuotientSafety
target_anti_qegtb = """               -- 「配置空間がQEGTBにない」安全空間確保の基本ボーナス！
               safeNonQegtbBonus = 850.0
           in safeNonQegtbBonus + escapesBonus + threatReductionBonus + friendlySupportBonus"""

new_evals = """               -- 「配置空間がQEGTBにない」安全空間確保の基本ボーナス！
               safeNonQegtbBonus = 850.0
           in safeNonQegtbBonus + escapesBonus + threatReductionBonus + friendlySupportBonus

-- | 飛車・金（および全駒）の目的のない往復移動（Shuffling / Ping-pong）、
--   千日手未遂（2回目・3回目の同一局面）、彷徨い移動（うろうろ手損）の検知とペナルティ
evalPieceShuffling :: State -> Move -> Double
evalPieceShuffling st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      hist = history st
      mHist = moveHistory st
      nextSt = applyMove m st
      nextBd = board nextSt
      nextKey = positionKey nextSt
      
      -- 自玉が王手されている時の回避手はペナルティ対象外
      inCheck = isKingUnderAttack plr curBd
      
  in if inCheck
     then 0.0
     else
       let
         -- 1. 同一局面リピート（千日手未遂・Shuffling）の段階的ペナルティ
         repeatCount = Map.findWithDefault 0 nextKey hist
         repeatPen = case repeatCount of
           1 -> -800.0   -- 2回目の出現：明らかな手待ち・千日手未遂に強いペナルティ
           2 -> -3500.0  -- 3回目の出現：直ちに千日手になるため超特大ペナルティ
           _ -> if repeatCount >= 3 then -10000.0 else 0.0
         
         -- 2. 直前の自分の手（2手前）からの直接逆戻り（Immediate Oscillation）検知
         reversalPen = case reverse mHist of
           (_oppMv : myLastMvStr : _) ->
             case (m, parseMove myLastMvStr) of
               (NormalMove curFrom curTo _, NormalMove prevFrom prevTo _) ->
                 if curFrom == prevTo && curTo == prevFrom
                 then
                   let isTargetUnderThreat = not (null (getPiecesCovering opp curFrom curBd))
                       isCapture = Map.member curTo curBd
                       pieceAtFrom = Map.lookup curFrom curBd
                       isRookOrGold = case pieceAtFrom of
                         Just (Piece _ pt) -> pt `elem` [Rook, PRook, Gold, PPawn, PSilver, PKnight, PLance]
                         Nothing           -> False
                   in if isTargetUnderThreat || isCapture
                      then 0.0
                      else if isRookOrGold
                           then -2500.0 -- 飛車・金の無意味な往復移動（うろうろ）には特大ペナルティ！
                           else -1500.0 -- 一般駒の無意味な往復手損ペナルティ
                 else 0.0
               _ -> 0.0
           _ -> 0.0
         
         -- 3. 金の彷徨い（Wandering Gold）検知：
         --    金は玉の守り。敵玉攻めや駒取りでないのに自玉から離れる手、中段に浮く手を減点
         wanderingGoldPen = case m of
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
           _ -> 0.0
         
         -- 4. 飛車のうろうろ（Aimless Rook Wandering）検知：
         aimlessRookPen = case m of
           NormalMove from to promo ->
             case Map.lookup from curBd of
               Just (Piece p Rook) | p == plr ->
                 let isCapturing = Map.member to curBd
                     isCheck = isKingUnderAttack opp nextBd
                     isPromo = promo
                     isPointlessLift = (plr == Sente && from == (2, 8) && (to == (2, 7) || to == (2, 6)) &&
                                        Map.lookup (2, 7) curBd == Just (Piece plr Pawn)) ||
                                       (plr == Gote && from == (8, 2) && (to == (8, 3) || to == (8, 4)) &&
                                        Map.lookup (8, 3) curBd == Just (Piece plr Pawn))
                 in if isCapturing || isCheck || isPromo
                    then 0.0
                    else if isPointlessLift then -800.0 else 0.0
               _ -> 0.0
           _ -> 0.0
           
       in repeatPen + reversalPen + wanderingGoldPen + aimlessRookPen

-- | プロ棋戦の手筋に基づく「受けの筋（Defensive Techniques）」の評価
-- 1. 自陣への歩打ち受け（底歩、中合い、飛車先交換後の歩受け P*2c, P*8g 等）
-- 2. 金銀の引き受け・玉との連結（「金は引く手に好手あり」、玉に密着する守備）
-- 3. 相手の攻撃線（射線・成込み）を遮断する合駒・受け
-- 4. 自陣の角打ち隙（両取りの急所）を消す手筋
evalDefensiveTechnique :: State -> Move -> Double
evalDefensiveTechnique st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      nextSt = applyMove m st
      nextBd = board nextSt
      
      curKingSq = findKing plr curBd
      nextKingSq = findKing plr nextBd
      
      -- 1. 歩打ちによる受け（Pawn Drop Defense）
      pawnDropDefenseScore = case m of
        DropMove Pawn (tx, ty) ->
          let isInMyCamp = if plr == Sente then ty >= 7 else ty <= 3
              oppAttackers = getPiecesCovering opp (tx, ty) curBd
              hasEnemyRanger = any (\(_, Piece _ opt) -> opt `elem` [Rook, PRook, Bishop, PBishop, Lance]) oppAttackers
              isRookHeadPawn = (plr == Gote && (tx, ty) == (2, 3)) || (plr == Sente && (tx, ty) == (8, 7))
          in if isRookHeadPawn then 500.0
             else if isInMyCamp && hasEnemyRanger then 420.0
             else if isInMyCamp then 180.0
             else 0.0
        _ -> 0.0
        
      -- 2. 金銀の引き受け・玉との連結（Gold/Silver Retreat & Solidification）
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
                  defendsFriendly = any (\(sq, Piece own _) -> own == plr && isSquareUnderAttack opp sq curBd && isSquareUnderAttack plr sq nextBd) (Map.toList nextBd)
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
          
  in pawnDropDefenseScore + solidDefenseScore + antiForkDefenseScore"""

if target_anti_qegtb in content:
    content = content.replace(target_anti_qegtb, new_evals, 1)
    print("1. Added evalPieceShuffling and evalDefensiveTechnique")
else:
    print("ERROR: target_anti_qegtb not found!")

# 2. Add isShufflingBlunder to isBlunderMove
target_blunder = """          isSelfSuicideQegtb = (isKingMove || isNearKing) &&
            case lookupQuotientTablebase nextSt of
              Just (1, _) -> True
              _           -> False
      in if isMajorSuicideCheck || isMajorCheapTradeCheck || isSelfSuicideQegtb
         then True -- 大駒のただ捨て王手・大損交換王手・自爆QEGTB飛び込みは100%絶対大悪手！"""

new_blunder = """          isSelfSuicideQegtb = (isKingMove || isNearKing) &&
            case lookupQuotientTablebase nextSt of
              Just (1, _) -> True
              _           -> False
              
          -- 6. 理由のない飛車・金・銀の直接往復移動（うろうろ手損）の完全排除
          isShufflingBlunder =
            case reverse (moveHistory st) of
              (_oppMv : myLastMvStr : _) ->
                case (m, parseMove myLastMvStr) of
                  (NormalMove curFrom curTo _, NormalMove prevFrom prevTo _) ->
                    curFrom == prevTo && curTo == prevFrom &&
                    null (getPiecesCovering opp curFrom bd) &&
                    not (Map.member curTo bd) &&
                    (case Map.lookup curFrom bd of
                       Just (Piece _ pt) -> pt `elem` [Rook, PRook, Gold, Silver]
                       Nothing           -> False)
                  _ -> False
              _ -> False
      in if isMajorSuicideCheck || isMajorCheapTradeCheck || isSelfSuicideQegtb || isShufflingBlunder
         then True -- 大駒のただ捨て王手・大損交換王手・自爆QEGTB・飛車金うろうろ往復は100%絶対大悪手！"""

if target_blunder in content:
    content = content.replace(target_blunder, new_blunder, 1)
    print("2. Added isShufflingBlunder to isBlunderMove")
else:
    print("ERROR: target_blunder not found!")

# 3. Add to scoreMove
target_scoremove = """                    -- ★ 自玉周りの指し手において、駒の配置空間が相手のQEGTBにない局面になる手を最優先！
                    antiQuotientSafetyBonus = evalAntiQuotientSafety st m
                    
                    s = evaluateState weights (turn st) nextSt
                in s + blunderPen + lancePen + checkBonus + forkBonus + tacticalBonus
                   + discoveredBonus + rayUnblockBonus + potentialAlignBonus + quotientEquivBonus
                    + antiQuotientSafetyBonus"""

new_scoremove = """                    -- ★ 自玉周りの指し手において、駒の配置空間が相手のQEGTBにない局面になる手を最優先！
                    antiQuotientSafetyBonus = evalAntiQuotientSafety st m
                    
                    -- ★ 飛車・金などの無駄なうろうろ手損・往復移動・千日手未遂への厳罰ペナルティ
                    shufflingPen = evalPieceShuffling st m
                    
                    -- ★ プロ棋戦標準の「受けの筋（歩打ち・金銀引き受け・角打ち隙消去）」ボーナス
                    defensiveBonus = evalDefensiveTechnique st m
                    
                    s = evaluateState weights (turn st) nextSt
                in s + blunderPen + lancePen + checkBonus + forkBonus + tacticalBonus
                   + discoveredBonus + rayUnblockBonus + potentialAlignBonus + quotientEquivBonus
                   + antiQuotientSafetyBonus + shufflingPen + defensiveBonus"""

if target_scoremove in content:
    content = content.replace(target_scoremove, new_scoremove, 1)
    print("3. Added shufflingPen and defensiveBonus to scoreMove")
else:
    print("ERROR: target_scoremove not found!")

# 4. Update Sennichite threshold to >= 3
content = content.replace("case Map.lookup key hist of\n    Just count -> count >= 4", "case Map.lookup key hist of\n    Just count -> count >= 3")
print("4. Updated Sennichite threshold to >= 3")

with open('c:\\VS\\Workspace\\t-shogi\\ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("Saved updated ShogiUsiMorse-v4.hs")
