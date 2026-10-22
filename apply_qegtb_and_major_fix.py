with open("ShogiUsiMorse-v4.hs", "r", encoding="utf-8") as f:
    content = f.read()

# 1. evalQuotientEquivalence を lookupQuotientTablebase の後に追加
target1 = "             matches = [ res | pat <- endgameTablebase, Just res <- [matchPattern pat] ]\n         in listToMaybe matches"

replacement1 = target1 + """

-- | 指し手を指した結果の局面が、QEGTB（商空間エンドゲームテーブル）の配置空間と同値であるか判定・評価
-- 1. 指した後の局面で自分手番と仮定した場合にQEGTB即詰み（DTM 1 または 3）になる手（詰めろ・QEGTB直接誘導）
-- 2. 相手玉の脱出可能トポロジー（Escape Topology）を縮約し、頭金・腹金・吊るし桂の前提空間へと収束させる手
-- 3. 相手玉の隣接マスに味方の紐（支え）を新設してQEGTBイデアルを構築する手
evalQuotientEquivalence :: State -> Move -> Double
evalQuotientEquivalence st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      nextSt = applyMove m st
      nextBd = board nextSt
      
      -- 1. 指し手を指した後の局面で、自分が手番なら即座にQEGTB即詰み（DTM 1 または 3）になるか（詰めろ・QEGTB同値空間への直接突入）
      stHypotheticalMyTurn = nextSt { turn = plr }
      qegtbDirectHitScore = case lookupQuotientTablebase stHypotheticalMyTurn of
        Just (dtm, _) ->
          -- DTM 1（頭金・腹金・吊るし桂等の即詰み同値空間に遷移）なら超特大ボーナス！
          if dtm == 1 then 1800.0 else 1200.0
        Nothing       -> 0.0
      
      -- 2. 相手玉の脱出トポロジー（Escape Topology）の縮約度
      escapeReductionScore =
        case (findKing opp curBd, findKing opp nextBd) of
          (Just kCur, Just kNext) ->
            let curEscapes = length (getKingEscapes opp kCur curBd)
                nextEscapes = length (getKingEscapes opp kNext nextBd)
                -- 脱出可能マスが0（完全包囲・退路なし）になった場合
                zeroEscapeBonus = if nextEscapes == 0 then 600.0 else 0.0
                -- 脱出可能マスを狭めた（トポロジー縮約）ボーナス
                reducedBonus = if nextEscapes < curEscapes
                               then fromIntegral (curEscapes - nextEscapes) * 220.0
                               else 0.0
            in zeroEscapeBonus + reducedBonus
          _ -> 0.0
      
      -- 3. QEGTBイデアル構築（相手玉の隣接マスへの支え・紐の創出、頭金・腹金スペースの確保）
      idealSupportScore =
        case findKing opp nextBd of
          Just (kx, ky) ->
            let neighbors = [ (kx + dx, ky + dy)
                            | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)
                            , not (isOutOfBounds (kx + dx, ky + dy))
                            ]
                -- 着手後に新たに味方の支え（紐）がついた隣接マスの数
                newSupportedNeighbors = length [ sq | sq <- neighbors
                                                    , isSquareUnderAttack plr sq nextBd
                                                    , not (isSquareUnderAttack plr sq curBd) ]
            in fromIntegral newSupportedNeighbors * 150.0
          Nothing -> 0.0
  in qegtbDirectHitScore + escapeReductionScore + idealSupportScore"""

if target1 not in content:
    print("Error: target1 not found!")
    exit(1)

content = content.replace(target1, replacement1, 1)

# 2. isTacticalMove の更新（QEGTB同値手を静止探索対象に含める）
target2 = """          isRayUnblock = case m of
            NormalMove from _ _ ->
              any (\\(bsq, Piece bp bpt) -> bp == plr && bpt `elem` [Rook, PRook, Bishop, PBishop, Lance] && canPieceXRayTarget bsq (Piece bp bpt) from (board s)) (Map.toList (board s))
            _ -> False
      in case m of
           -- 1. 相手の駒を取る手、大駒成り、王手、両取り、数の攻め突破、射線開通
           NormalMove _ to promo ->
             Map.member to (board s) || promo || isCheck || isFork || isBreakthrough || isRayUnblock
           -- 2. 持ち駒の打ち込み（王手、両取り、敵玉周辺への打ち込み、急所打ち込み、数の攻め突破）
           DropMove pt to ->
             isCheck || isFork || isBreakthrough || (case findKing opp (board s) of
                           Just (kx, ky) -> abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 && pt `elem` [Gold, Silver, Rook, Bishop, Knight, Pawn]
                           Nothing -> False)"""

replacement2 = """          isRayUnblock = case m of
            NormalMove from _ _ ->
              any (\\(bsq, Piece bp bpt) -> bp == plr && bpt `elem` [Rook, PRook, Bishop, PBishop, Lance] && canPieceXRayTarget bsq (Piece bp bpt) from (board s)) (Map.toList (board s))
            _ -> False
          -- QEGTBの配置空間と同値になる手（詰めろ、または相手玉脱出マス0）
          isQEGTBEquiv = evalQuotientEquivalence s m >= 500.0
      in case m of
           -- 1. 相手の駒を取る手、大駒成り、王手、両取り、数の攻め突破、射線開通、QEGTB同値手
           NormalMove _ to promo ->
             Map.member to (board s) || promo || isCheck || isFork || isBreakthrough || isRayUnblock || isQEGTBEquiv
           -- 2. 持ち駒の打ち込み（王手、両取り、敵玉周辺への打ち込み、急所打ち込み、数の攻め突破、QEGTB同値手）
           DropMove pt to ->
             isCheck || isFork || isBreakthrough || isQEGTBEquiv || (case findKing opp (board s) of
                           Just (kx, ky) -> abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 && pt `elem` [Gold, Silver, Rook, Bishop, Knight, Pawn]
                           Nothing -> False)"""

if target2 not in content:
    print("Error: target2 not found!")
    exit(1)

content = content.replace(target2, replacement2, 1)

# 3. scoreMove の更新（大駒のタダ捨て王手ペナルティ & QEGTB配置空間同値ボーナス）
target3 = """                    nextSt = applyMove m st
                    opp = opponent (turn st)
                    nextBd = board nextSt
                    isCheck = isKingUnderAttack opp nextBd
                    checkBonus = if isCheck then 400.0 else 0.0
                    forkBonus = if isForkMove st m then 450.0 else 0.0"""

replacement3 = """                    nextSt = applyMove m st
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

# 4. tacticalBonus と 合計スコア s + ... に quotientEquivBonus を追加
target4 = """                    tacticalBonus = case m of"""
replacement4 = """                    tacticalBonus = if isMajorSuicideCheck || isMajorCheapTradeCheck then 0.0 else case m of"""

if target4 not in content:
    print("Error: target4 not found!")
    exit(1)

content = content.replace(target4, replacement4, 1)

target5 = """               in s + blunderPen + lancePen + checkBonus + forkBonus + tacticalBonus
                  + discoveredBonus + rayUnblockBonus + potentialAlignBonus"""

replacement5 = """               in s + blunderPen + lancePen + checkBonus + forkBonus + tacticalBonus
                  + discoveredBonus + rayUnblockBonus + potentialAlignBonus + quotientEquivBonus"""

if target5 not in content:
    print("Error: target5 not found!")
    exit(1)

content = content.replace(target5, replacement5, 1)

with open("ShogiUsiMorse-v4.hs", "w", encoding="utf-8") as f:
    f.write(content)

print("Successfully applied all modifications!")
