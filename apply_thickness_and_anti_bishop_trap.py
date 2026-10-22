with open('c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

# 1. Update isBlunderMove:
# Find isShufflingBlunder block
idx_sb = content.find("isShufflingBlunder =")
if idx_sb != -1:
    idx_end = content.find("in if isMajorSuicideCheck", idx_sb)
    idx_then = content.find("then True", idx_end)
    idx_line_end = content.find("\n", idx_then)
    
    # We replace from idx_sb to idx_line_end
    old_block = content[idx_sb:idx_line_end]
    new_block = """isShufflingBlunder =
            case reverse (moveHistory st) of
              (_oppMv : myLastMvStr : _) ->
                case (m, parseMove myLastMvStr) of
                  (NormalMove curFrom curTo _, NormalMove prevFrom prevTo _) ->
                    curFrom == prevTo && curTo == prevFrom &&
                    null (getPiecesCovering opp curFrom bd) &&
                    not (Map.member curTo bd) &&
                    (case Map.lookup curFrom bd of
                       Just (Piece _ pt) -> pt `elem` [Rook, PRook, Gold, Silver, King]
                       Nothing           -> False)
                  _ -> False
              _ -> False
              
          -- 7. 相手の手持ち角による「8八（先手）/ 2二（後手）への角打ち・香車取り馬作り」の隙を自ら生み出す大失着の完全排除
          isExposingCornerBishopBlunder =
            let oppHand = if plr == Sente then goteHand st else senteHand st
                hasOppBishopInHand = Map.findWithDefault 0 Bishop oppHand > 0
                (cornerSq, lanceSq) = if plr == Sente then ((8, 8), (9, 9)) else ((2, 2), (1, 1))
                isCornerEmpty = not (Map.member cornerSq nextBd)
                isCornerUnguarded = null (getPiecesCovering plr cornerSq nextBd)
                isLanceTarget = case Map.lookup lanceSq nextBd of
                  Just (Piece ppt pt) -> ppt == plr && pt == Lance
                  Nothing             -> False
                isExposed = hasOppBishopInHand && isCornerEmpty && isCornerUnguarded && isLanceTarget
            in isExposed && not isCheck && not isCapturingValuable
            
          -- 8. 玉の目的のない危険な中央ふらふら移動（Wandering King Blunder）
          isWanderingKingBlunder =
            case m of
              NormalMove from to _ ->
                case Map.lookup from bd of
                  Just (Piece p King) | p == plr ->
                    let isMovingToCenter = snd to == 5 || (plr == Sente && snd to == 6) || (plr == Gote && snd to == 4)
                        hasNoThreat = null (getPiecesCovering opp from bd)
                    in isMovingToCenter && hasNoThreat && not isCheck
                  _ -> False
              _ -> False
      in if isMajorSuicideCheck || isMajorCheapTradeCheck || isSelfSuicideQegtb || isShufflingBlunder || isExposingCornerBishopBlunder || isWanderingKingBlunder
         then True -- 大駒ただ捨て王手・大損交換・自爆QEGTB・飛車金玉うろうろ往復・88角打ち隙放置は100%絶対大悪手！"""
    content = content[:idx_sb] + new_block + content[idx_line_end:]
    print("1. isBlunderMove updated successfully")
else:
    print("1. isShufflingBlunder NOT found!")

# 2. Update evalPieceShuffling
idx_eps = content.find("evalPieceShuffling ::")
if idx_eps != -1:
    idx_isRookOrGold = content.find("isRookOrGold = case pieceAtFrom of", idx_eps)
    idx_in_if = content.find("in if isTargetUnderThreat || isCapture", idx_isRookOrGold)
    idx_end_rev = content.find("wanderingGoldPen =", idx_in_if)
    
    old_rev_block = content[idx_isRookOrGold:idx_end_rev]
    new_rev_block = """isRookOrGold = case pieceAtFrom of
                         Just (Piece _ pt) -> pt `elem` [Rook, PRook, Gold, PPawn, PSilver, PKnight, PLance]
                         Nothing           -> False
                       isKing = case pieceAtFrom of
                         Just (Piece _ King) -> True
                         _                   -> False
                   in if isTargetUnderThreat || isCapture
                      then 0.0
                      else if isKing
                           then -3500.0 -- 玉の無意味なうろうろ往復移動には超特大ペナルティ！
                           else if isRookOrGold
                           then -2500.0 -- 飛車・金の無意味な往復移動（うろうろ）には特大ペナルティ！
                           else -1500.0 -- 一般駒の無意味な往復手損ペナルティ
                 else 0.0
               _ -> 0.0
           _ -> 0.0
         
         -- 2.5 玉の彷徨い（Wandering King）防止と囲い入城（Castle Progress）ボーナス
         kingMoveEval = case m of
           NormalMove from to _ ->
             case Map.lookup from curBd of
               Just (Piece p King) | p == plr ->
                 let isMovingToCenter = snd to == 5 || (plr == Sente && snd to == 6) || (plr == Gote && snd to == 4)
                     wanderingPen = if isMovingToCenter then -2000.0 else 0.0
                     
                     -- 囲いの定位置（美濃・矢倉・雁木・エルモ・穴熊）
                     castleSquares = if plr == Sente
                                     then [(7, 8), (8, 8), (7, 9), (6, 8), (8, 9)]
                                     else [(3, 2), (2, 2), (3, 1), (4, 2), (2, 1)]
                     isEnteringCastle = to `elem` castleSquares
                     castleBonus = if isEnteringCastle then 450.0 else 0.0
                 in wanderingPen + castleBonus
               _ -> 0.0
           _ -> 0.0
         
         """
    content = content[:idx_isRookOrGold] + new_rev_block + content[idx_end_rev:]
    
    # Update return expression
    idx_ret = content.find("in repeatPen + reversalPen + wanderingGoldPen + aimlessRookPen", idx_eps)
    if idx_ret != -1:
        content = content[:idx_ret] + "in repeatPen + reversalPen + wanderingGoldPen + aimlessRookPen + kingMoveEval" + content[idx_ret + len("in repeatPen + reversalPen + wanderingGoldPen + aimlessRookPen"):]
        print("2. evalPieceShuffling updated successfully")
    else:
        print("2. return expression not found!")
else:
    print("2. evalPieceShuffling :: not found!")

# 3. Update evalDefensiveTechnique
idx_edt = content.find("evalDefensiveTechnique ::")
if idx_edt != -1:
    idx_tail = content.find("in pawnDropDefenseScore + solidDefenseScore + antiForkDefenseScore", idx_edt)
    if idx_tail != -1:
        new_def_tail = """      -- 4. ★ 8八（先手）/ 2二（後手）への角打ち・香車取り馬作り筋の事前完全防御
      antiCornerBishopDefenseScore =
        let oppHand = if plr == Sente then goteHand st else senteHand st
            hasOppBishopInHand = Map.findWithDefault 0 Bishop oppHand > 0
            (cornerSq, lanceSq) = if plr == Sente then ((8, 8), (9, 9)) else ((2, 2), (1, 1))
            
            -- 移動後の局面での防御状況
            isCornerOccupied = Map.member cornerSq nextBd
            cornerDefenders = getPiecesCovering plr cornerSq nextBd
            isCornerSafelyGuarded = not (null cornerDefenders)
            
            -- 現在の局面で隙があったか
            wasCornerEmpty = not (Map.member cornerSq curBd)
            wasCornerUnguarded = null (getPiecesCovering plr cornerSq curBd)
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
           else 0.0
          
  in pawnDropDefenseScore + solidDefenseScore + antiForkDefenseScore + antiCornerBishopDefenseScore

-- | 厚み（Thick Formations）と位取り（Dominance / Territorial Control）、囲いと駒組みの評価
-- 1. 中央（5筋・4筋・6筋）の位取り（歩の前進、相手を圧迫）
-- 2. 歩越し銀（銀が歩の上に前進して厚みを構築）
-- 3. 金銀のスクラム・連結（金と銀、銀と銀が互いに支え合って鉄壁を形成）
-- 4. 堅陣囲い（玉の周囲に金銀が連結配置される美しい駒組み）
evalThicknessAndDominance :: State -> Move -> Double
evalThicknessAndDominance st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      nextSt = applyMove m st
      nextBd = board nextSt
      
      -- 1. 中央および要所の位取り（Territorial Control）
      territoryScore = case m of
        NormalMove _ (tx, ty) _ ->
          let isVanguardPawn = case Map.lookup (tx, ty) nextBd of
                Just (Piece p Pawn) -> p == plr
                _                   -> False
              isCenterRank = if plr == Sente
                             then (tx == 5 && ty == 5) || (tx == 5 && ty == 6) || (tx `elem` [4, 6] && ty == 6)
                             else (tx == 5 && ty == 5) || (tx == 5 && ty == 4) || (tx `elem` [4, 6] && ty == 4)
              isDeepRank   = if plr == Sente then (tx == 5 && ty == 5) else (tx == 5 && ty == 5)
          in if isVanguardPawn && isDeepRank then 380.0
             else if isVanguardPawn && isCenterRank then 260.0
             else 0.0
        _ -> 0.0
        
      -- 2. 歩越し銀（Silver vanguard building thickness）
      silverVanguardScore = case m of
        NormalMove _ (tx, ty) _ ->
          case Map.lookup (tx, ty) nextBd of
            Just (Piece p Silver) | p == plr ->
              let pawnBelow = if plr == Sente then (tx, ty + 1) else (tx, ty - 1)
                  hasPawnBelow = Map.lookup pawnBelow nextBd == Just (Piece plr Pawn)
                  isForwardRank = if plr == Sente then ty <= 6 else ty >= 4
              in if hasPawnBelow && isForwardRank then 320.0 else 0.0
            _ -> 0.0
        _ -> 0.0
        
      -- 3. 金銀の連結・スクラム（Gold-Silver Connection）
      scrumScore = case m of
        NormalMove _ to _ ->
          case Map.lookup to nextBd of
            Just (Piece p pt) | p == plr && pt `elem` [Gold, Silver] ->
              let (tx, ty) = to
                  neighbors = [ (tx + dx, ty + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]
                  adjacentGenerals = length [ sq | sq <- neighbors
                                                 , case Map.lookup sq nextBd of
                                                     Just (Piece p2 pt2) -> p2 == plr && pt2 `elem` [Gold, Silver]
                                                     _                   -> False ]
              in fromIntegral adjacentGenerals * 150.0
            _ -> 0.0
        _ -> 0.0
        
      -- 4. 囲いの完成度・玉との連結度（Castle Formation & Coordination）
      curKingSq = findKing plr curBd
      nextKingSq = findKing plr nextBd
      castleScore = case nextKingSq of
        Just (kx, ky) ->
          let neighbors = [ (kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]
              protectingGenerals = length [ sq | sq <- neighbors
                                               , case Map.lookup sq nextBd of
                                                   Just (Piece p pt) -> p == plr && pt `elem` [Gold, Silver]
                                                   _                 -> False ]
          in fromIntegral protectingGenerals * 160.0
        Nothing -> 0.0
        
  in territoryScore + silverVanguardScore + scrumScore + castleScore"""
        content = content[:idx_tail] + new_def_tail + content[idx_tail + len("in pawnDropDefenseScore + solidDefenseScore + antiForkDefenseScore"):]
        print("3. evalDefensiveTechnique and evalThicknessAndDominance added")
    else:
        print("3. tail not found in evalDefensiveTechnique")
else:
    print("3. evalDefensiveTechnique :: not found")

# 4. Update scoreMove to add thicknessBonus
idx_sm = content.find("defensiveBonus = evalDefensiveTechnique st m")
if idx_sm != -1:
    idx_sm_end = content.find("+ antiQuotientSafetyBonus + shufflingPen + defensiveBonus", idx_sm)
    if idx_sm_end != -1:
        orig_str = "+ antiQuotientSafetyBonus + shufflingPen + defensiveBonus"
        new_str = "+ antiQuotientSafetyBonus + shufflingPen + defensiveBonus + evalThicknessAndDominance st m"
        content = content[:idx_sm_end] + new_str + content[idx_sm_end + len(orig_str):]
        print("4. evalThicknessAndDominance integrated into scoreMove")
    else:
        print("4. scoreMove addition line not found")
else:
    print("4. scoreMove not found")

with open('c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("FILE SUCCESSFULLY WRITTEN!")
