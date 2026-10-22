with open("ShogiUsiMorse-v4.hs", "r", encoding="utf-8") as f:
    content = f.read()

# 3. scoreMove の更新（大駒のタダ捨て王手ペナルティ & QEGTB配置空間同値ボーナス）
target3 = """                   nextSt = applyMove m st
                   opp = opponent (turn st)
                   nextBd = board nextSt
                   isCheck = isKingUnderAttack opp nextBd
                   checkBonus = if isCheck then 400.0 else 0.0
                   forkBonus = if isForkMove st m then 450.0 else 0.0"""

replacement3 = """                   nextSt = applyMove m st
                   opp = opponent (turn st)
                   nextBd = board nextSt
                   isCheck = isKingUnderAttack opp nextBd
                   
                   -- ★ 大駒のただ捨て王手の完全検知と強烈ペナルティ排除
                   (toSq, ptMoved, isPromo) = case m of
                     NormalMove _ t p ->
                       case Map.lookup t nextBd of
                         Just (Piece _ pt) -> (t, pt, p)
                         Nothing           -> (t, Pawn, False)
                     DropMove pt t    -> (t, pt, False)
                   isMajor = ptMoved `elem` [Rook, PRook, Bishop, PBishop]
                   oppAttackers = getPiecesCovering opp toSq nextBd
                   friendlyDefenders = getPiecesCovering (turn st) toSq nextBd
                   targetCaptured = Map.lookup toSq (board st)
                   isCapturingValuable = case targetCaptured of
                     Just (Piece _ capPt) -> pieceValue capPt >= 350.0
                     Nothing              -> False
                   
                   -- 紐なしで敵の利きのマスに大駒で飛び込む王手（タダ取られ王手）
                   isMajorSuicideCheck = isCheck && isMajor && not (null oppAttackers) && null friendlyDefenders && not isCapturingValuable
                   -- 相手の歩・香・桂・銀などの安い駒で大駒を取られる大損交換王手
                   hasCheapAttacker = any (\\(_, Piece _ opt) -> opt `elem` [Pawn, Lance, Knight, Silver]) oppAttackers
                   isMajorCheapTradeCheck = isCheck && isMajor && hasCheapAttacker && not isCapturingValuable
                   
                   -- 大駒のタダ捨て王手・大損交換王手なら、王手ボーナスをゼロにし、-10000点ペナルティ！
                   checkBonus = if isMajorSuicideCheck || isMajorCheapTradeCheck
                                then -10000.0
                                else if isCheck then 400.0 else 0.0
                   forkBonus = if isForkMove st m then 450.0 else 0.0
                   
                   -- ★ QEGTB（商空間エンドゲームテーブル）の配置空間と同値になる手への優先ボーナス！
                   quotientEquivBonus = evalQuotientEquivalence st m"""

if target3 not in content:
    print("Error: target3 not found!")
    exit(1)

content = content.replace(target3, replacement3, 1)

# 4. tacticalBonus
target4 = "                   tacticalBonus = case m of"
replacement4 = "                   tacticalBonus = if isMajorSuicideCheck || isMajorCheapTradeCheck then 0.0 else case m of"

if target4 not in content:
    print("Error: target4 not found!")
    exit(1)

content = content.replace(target4, replacement4, 1)

# 5. 合計点に quotientEquivBonus を追加
target5 = "                  + discoveredBonus + rayUnblockBonus + potentialAlignBonus"
replacement5 = "                  + discoveredBonus + rayUnblockBonus + potentialAlignBonus + quotientEquivBonus"

if target5 not in content:
    print("Error: target5 not found!")
    exit(1)

content = content.replace(target5, replacement5, 1)

with open("ShogiUsiMorse-v4.hs", "w", encoding="utf-8") as f:
    f.write(content)

print("Successfully applied scoreMove modifications!")
