import re

with open(r'c:\VS\Workspace\t-shogi\ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

# 1. MidgameWeights に wNumericalAssault を追加
target_weights = """data MidgameWeights = MidgameWeights
  { wMaterial       :: !Double -- 駒得重み (標準 1.0)
  , wMorseDanger    :: !Double -- モース玉ポテンシャル重み (標準 6.0)
  , wAttack         :: !Double -- 中盤攻め筋（成駒・玉頭圧力・拠点）重み (標準 1.3)
  , wDefense        :: !Double -- 中盤受け筋（金銀囲い・連結・敵成駒排除）重み (標準 1.3)
  , wKakuto         :: !Double -- 序盤角頭・桂頭防衛重み (標準 1.0)
  , wLoosePenalty   :: !Double -- 駒損（放れ駒・大損交換）ペナルティ重み (標準 1.2)
  , wPieceControl   :: !Double -- 駒の利き（大駒射線・銀桂攻撃参加・マス支配）重み (標準 1.5)
  , wTempoLoss      :: !Double -- 手得・手損（18香無駄上がり・飛車先不突き金寄り等の手損排除、手得追求）重み (標準 2.0)
  , wGoldKingCoord  :: !Double -- 金と玉の連携（金玉近接・守護スクラム）重み (標準 1.5)
  , wDistanceToMate :: !Double -- 詰み筋までの距離（DTM短縮・QEGTB接続）重み (標準 5.0)
  } deriving (Show, Read, Eq)

defaultWeights :: MidgameWeights
defaultWeights = MidgameWeights
  { wMaterial       = 1.0
  , wMorseDanger    = 6.0
  , wAttack         = 1.3
  , wDefense        = 1.3
  , wKakuto         = 1.0
  , wLoosePenalty   = 1.2
  , wPieceControl   = 1.5
  , wTempoLoss      = 2.0
  , wGoldKingCoord  = 1.5
  , wDistanceToMate = 5.0
  }"""

new_weights = """data MidgameWeights = MidgameWeights
  { wMaterial         :: !Double -- 駒得重み (標準 1.0)
  , wMorseDanger      :: !Double -- モース玉ポテンシャル重み (標準 6.0)
  , wAttack           :: !Double -- 中盤攻め筋（成駒・玉頭圧力・拠点）重み (標準 1.3)
  , wDefense          :: !Double -- 中盤受け筋（金銀囲い・連結・敵成駒排除）重み (標準 1.3)
  , wKakuto           :: !Double -- 序盤角頭・桂頭防衛重み (標準 1.0)
  , wLoosePenalty     :: !Double -- 駒損（放れ駒・大損交換）ペナルティ重み (標準 1.2)
  , wPieceControl     :: !Double -- 駒の利き（大駒射線・銀桂攻撃参加・マス支配）重み (標準 1.5)
  , wTempoLoss        :: !Double -- 手得・手損（18香無駄上がり・飛車先不突き金寄り等の手損排除、手得追求）重み (標準 2.0)
  , wGoldKingCoord    :: !Double -- 金と玉の連携（金玉近接・守護スクラム）重み (標準 1.5)
  , wDistanceToMate   :: !Double -- 詰み筋までの距離（DTM短縮・QEGTB接続）重み (標準 5.0)
  , wNumericalAssault :: !Double -- 数の攻め（特定地点への利きの集中・突破、局所的数の優位）重み (標準 2.0)
  } deriving (Show, Read, Eq)

defaultWeights :: MidgameWeights
defaultWeights = MidgameWeights
  { wMaterial         = 1.0
  , wMorseDanger      = 6.0
  , wAttack           = 1.3
  , wDefense          = 1.3
  , wKakuto           = 1.0
  , wLoosePenalty     = 1.2
  , wPieceControl     = 1.5
  , wTempoLoss        = 2.0
  , wGoldKingCoord    = 1.5
  , wDistanceToMate   = 5.0
  , wNumericalAssault = 2.0
  }"""

# 2. evalNumericalAssault を追加
eval_assault_code = """-- | 5. 数の攻め評価（特定地点への利きの集中・突破、および局所的な数の優位）
evalNumericalAssault :: Player -> State -> Double
evalNumericalAssault plr st =
  let bd = board st
      opp = opponent plr
      oppKingSq = findKing opp bd
      
      -- 重要ターゲットマス：
      -- 1. 敵玉の周囲8マス（玉頭、玉の横）
      -- 2. 敵の守備駒（金、銀、角、飛、歩）がいるマス
      targetSquares =
        let kingNeighbors = case oppKingSq of
              Just (kx, ky) -> [ (kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0), not (isOutOfBounds (kx + dx, ky + dy)) ]
              Nothing       -> []
            enemyDefenders = [ sq | (sq, Piece p pt) <- Map.toList bd, p == opp, pt `elem` [Gold, Silver, Pawn, Knight] ]
        in nub (kingNeighbors ++ enemyDefenders)
      
      -- 各ターゲットマスにおける数の攻めの成否を評価
      evalSquare sq =
        let myAttackers = getPiecesCovering plr sq bd
            oppDefenders = getPiecesCovering opp sq bd
            nAtt = length myAttackers
            nDef = length oppDefenders
        in if nAtt == 0
           then 0.0
           else
             -- A. 数の優位（味方の利き数が相手の利き数を上回る：突破確定！）
             let breakBonus = if nAtt > nDef then fromIntegral (nAtt - nDef) * 60.0 else 0.0
                 -- B. 攻めの集中（2枚以上の利きが同じマスに集約されているバッテリー・数の攻め）
                 concentrationBonus = if nAtt >= 2 then fromIntegral nAtt * 35.0 else 0.0
                 -- C. 敵玉頭・敵玉隣接への集中攻撃ボーナス
                 kingHeadBonus = case oppKingSq of
                   Just (kx, ky) ->
                     let dir = if opp == Sente then -1 else 1
                     in if sq == (kx, ky + dir) && nAtt >= 1 then fromIntegral nAtt * 50.0 else 0.0
                   Nothing -> 0.0
             in breakBonus + concentrationBonus + kingHeadBonus
  in sum [ evalSquare sq | sq <- targetSquares ]

"""

# 3. isBlunderMove の刷新（大駒の成り捨て・切り捨て・数の攻め突破を救出）
old_blunder = """-- | 移動先マスが相手の利きに晒されており、かつ味方の紐が付いていない（または安い駒で取られる）大悪手・タダ捨て手か判定
isBlunderMove :: State -> Move -> Bool
isBlunderMove st m =
  let plr = turn st
      opp = opponent plr
      bd = board st
      (toSq, ptMoved) = case m of
        NormalMove from to _ ->
          case Map.lookup from bd of
            Just (Piece _ pt) -> (to, pt)
            Nothing           -> (to, Pawn)
        DropMove pt to       -> (to, pt)
      
      -- 移動先マスにおける相手の利き駒一覧
      oppAttackers = getPiecesCovering opp toSq bd
      -- 移動後にそのマスに味方の利き（紐）が付いているか
      nextBd = board (applyMove m st)
      friendlyDefenders = [ fromSq | (fromSq, Piece p _) <- Map.toList nextBd
                                   , p == plr, fromSq /= toSq
                                   , canPieceReachTarget fromSq (Piece plr ptMoved) toSq nextBd ]
  in if null oppAttackers
     then False
     else
       let minOppVal = minimum [ pieceValue oppPt | (_, Piece _ oppPt) <- oppAttackers ]
           movedVal  = pieceValue ptMoved
       in if null friendlyDefenders
          then ptMoved /= Pawn -- 紐なしで歩以外の駒（銀、金、飛、角、桂、香）を相手の利きに飛び込ませるのは絶対タダ捨て！
          else movedVal > minOppVal + 250.0 -- 紐があっても、歩などで金銀飛角を取られる大損交換は悪手"""

new_blunder = """-- | 移動先マスが相手の利きに晒されており、かつ味方の紐が付いていない（または安い駒で取られる）大悪手・タダ捨て手か判定
-- ★ ただし、王手をかける手、敵の守備駒（金銀飛角など）を捕獲する手、敵陣での大駒の成り捨て（大攻勢）、
--    数の攻め（味方の利き >= 相手の利き）による突破手は決してタダ捨て悪手ではない！
isBlunderMove :: State -> Move -> Bool
isBlunderMove st m =
  let plr = turn st
      opp = opponent plr
      bd = board st
      nextSt = applyMove m st
      
      -- 1. 王手（Check）をかける手は絶対にタダ捨て悪手ではない！（成り捨て王手など）
      isCheck = isKingUnderAttack opp (board nextSt)
      
      -- 2. 敵の守備駒（金銀飛角など）を捕獲する手も大駒切り・成り捨ての強襲手！
      (toSq, ptMoved, isPromo) = case m of
        NormalMove from to promo ->
          case Map.lookup from bd of
            Just (Piece _ pt) -> (to, pt, promo)
            Nothing           -> (to, Pawn, False)
        DropMove pt to       -> (to, pt, False)
      
      targetCaptured = Map.lookup toSq bd
      isCapturingValuable = case targetCaptured of
        Just (Piece _ capPt) -> pieceValue capPt >= 350.0 -- 金・銀・飛・角・桂・と金等の重要駒を捕獲
        Nothing              -> False
      
      -- 3. 敵陣での大駒（飛車・角）の成り捨て（龍・馬を作る強襲）
      isMajorPromotion = isPromo && ptMoved `elem` [Rook, Bishop]
      
      -- 4. 数の攻めが成立しているマス（味方の利き >= 相手の利き）への突入
      oppAttackers = getPiecesCovering opp toSq bd
      nextBd = board nextSt
      friendlyDefenders = getPiecesCovering plr toSq nextBd
      isNumericalSuperiority = length friendlyDefenders >= length oppAttackers
  in if isCheck || isCapturingValuable || (isMajorPromotion && not (null friendlyDefenders)) || isNumericalSuperiority
     then False -- これらは大駒の成り捨て・数の攻め・強襲手であり、タダ捨て悪手ではない！
     else
       if null oppAttackers
       then False
       else
         let minOppVal = minimum [ pieceValue oppPt | (_, Piece _ oppPt) <- oppAttackers ]
             movedVal  = pieceValue ptMoved
         in if null friendlyDefenders
            then ptMoved /= Pawn -- 紐なしで歩以外の駒を敵の利きに無意味に飛び込ませるのはタダ捨て
            else movedVal > minOppVal + 350.0 -- 紐があっても、歩で飛車を取られるような無謀な手"""

# 4. evaluateState に evalNumericalAssault を統合
old_eval = """      distanceToMateAdvantage = (evalDistanceToMate player st - evalDistanceToMate opp st) * wDistanceToMate weights
  in materialAdvantage + morseAdvantage + attackAdvantage + defenseAdvantage
     + kakutoKeitoAdvantage + looseAdvantage + pieceControlAdvantage
     + tempoLossAdvantage + goldKingCoordAdvantage + distanceToMateAdvantage"""

new_eval = """      distanceToMateAdvantage = (evalDistanceToMate player st - evalDistanceToMate opp st) * wDistanceToMate weights
      numericalAssaultAdvantage = (evalNumericalAssault player st - evalNumericalAssault opp st) * wNumericalAssault weights
  in materialAdvantage + morseAdvantage + attackAdvantage + defenseAdvantage
     + kakutoKeitoAdvantage + looseAdvantage + pieceControlAdvantage
     + tempoLossAdvantage + goldKingCoordAdvantage + distanceToMateAdvantage
     + numericalAssaultAdvantage"""

# 5. searchQuiescence の戦術手拡張（大駒成り・打ち込み・王手）
old_qs = """-- | 静止探索（駒の取り合いが落ち着くまで捕獲手を先読みする: 水平線効果の解消）
searchQuiescence :: MidgameWeights -> Int -> Double -> Double -> State -> Double
searchQuiescence weights 0 _ _ st = evaluateState weights (turn st) st
searchQuiescence weights qdepth alpha beta st =
  let standPat = evaluateState weights (turn st) st
  in if standPat >= beta
     then beta
     else
       let curAlpha = max alpha standPat
           rawMoves = generateMoves st
           captures = filter isCaptureMove rawMoves
           f5Captures = f5SyzygyFilter st captures
       in if null f5Captures
          then curAlpha
          else qLoop f5Captures curAlpha beta
  where
    isCaptureMove (NormalMove _ to _) = Map.member to (board st)
    isCaptureMove (DropMove _ _)       = False

    qLoop [] curA _ = curA
    qLoop (m:ms) curA b =
      let nextSt = applyMove m st
          score = - searchQuiescence weights (qdepth - 1) (-b) (-curA) nextSt
      in if score >= b
         then b
         else qLoop ms (max curA score) b"""

new_qs = """-- | 静止探索（捕獲手・王手・大駒成り・急所打ち込みを先読みして水平線効果を解消）
searchQuiescence :: MidgameWeights -> Int -> Double -> Double -> State -> Double
searchQuiescence weights 0 _ _ st = evaluateState weights (turn st) st
searchQuiescence weights qdepth alpha beta st =
  let standPat = evaluateState weights (turn st) st
  in if standPat >= beta
     then beta
     else
       let curAlpha = max alpha standPat
           rawMoves = generateMoves st
           tacticalMoves = filter (isTacticalMove st) rawMoves
           f5Tacticals = f5SyzygyFilter st tacticalMoves
       in if null f5Tacticals
          then curAlpha
          else qLoop f5Tacticals curAlpha beta
  where
    isTacticalMove s m =
      let opp = opponent (turn s)
          nextS = applyMove m s
          isCheck = isKingUnderAttack opp (board nextS)
      in case m of
           -- 1. 相手の駒を取る手、大駒成り、王手
           NormalMove _ to promo ->
             Map.member to (board s) || promo || isCheck
           -- 2. 持ち駒の打ち込み（王手、または敵玉周辺への打ち込み）
           DropMove pt to ->
             isCheck || (case findKing opp (board s) of
                           Just (kx, ky) -> abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 && pt `elem` [Gold, Silver, Rook, Bishop, Knight]
                           Nothing -> False)

    qLoop [] curA _ = curA
    qLoop (m:ms) curA b =
      let nextSt = applyMove m st
          score = - searchQuiescence weights (qdepth - 1) (-b) (-curA) nextSt
      in if score >= b
         then b
         else qLoop ms (max curA score) b"""

# 6. scoreMove で王手ボーナス、成りボーナス、打ち込みボーナスを追加
old_scoremove = """         let scoreMove m =
               let isBlunder = isBlunderMove st m
                   blunderPen = if isBlunder then -2000.0 else 0.0
                   isFutileLance = isFutileLanceLift st m
                   lancePen = if isFutileLance then -5000.0 else 0.0
                   
                   -- 飛車先の歩突き・角道オープンの最優先化
                   openBonus = case m of
                     NormalMove (2, 7) (2, 6) _ | turn st == Sente -> 280.0 -- 2六歩
                     NormalMove (2, 6) (2, 5) _ | turn st == Sente -> 240.0 -- 2五歩
                     NormalMove (8, 3) (8, 4) _ | turn st == Gote  -> 280.0 -- 8四歩
                     NormalMove (8, 4) (8, 5) _ | turn st == Gote  -> 240.0 -- 8五歩
                     NormalMove (7, 7) (7, 6) _ | turn st == Sente -> 220.0 -- 7六歩
                     NormalMove (3, 3) (3, 4) _ | turn st == Gote  -> 220.0 -- 3四歩
                     _ -> 0.0
                   
                   nextSt = applyMove m st
                   s = evaluateState weights (turn st) nextSt
               in s + blunderPen + lancePen + openBonus"""

new_scoremove = """         let scoreMove m =
               let isBlunder = isBlunderMove st m
                   blunderPen = if isBlunder then -2000.0 else 0.0
                   isFutileLance = isFutileLanceLift st m
                   lancePen = if isFutileLance then -5000.0 else 0.0
                   
                   nextSt = applyMove m st
                   opp = opponent (turn st)
                   isCheck = isKingUnderAttack opp (board nextSt)
                   checkBonus = if isCheck then 350.0 else 0.0
                   
                   -- 打ち込み・大駒の成り・飛車先歩突きボーナス
                   tacticalBonus = case m of
                     -- 大駒の成り
                     NormalMove _ _ True -> 180.0
                     -- 敵玉近傍への大駒・金銀の打ち込み
                     DropMove pt to ->
                       case findKing opp (board st) of
                         Just (kx, ky) | abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 ->
                           if pt `elem` [Gold, Silver, Rook, Bishop, Knight] then 260.0 else 90.0
                         _ -> 60.0
                     -- 飛車先歩突き・角道オープン
                     NormalMove (2, 7) (2, 6) _ | turn st == Sente -> 280.0
                     NormalMove (2, 6) (2, 5) _ | turn st == Sente -> 240.0
                     NormalMove (8, 3) (8, 4) _ | turn st == Gote  -> 280.0
                     NormalMove (8, 4) (8, 5) _ | turn st == Gote  -> 240.0
                     NormalMove (7, 7) (7, 6) _ | turn st == Sente -> 220.0
                     NormalMove (3, 3) (3, 4) _ | turn st == Gote  -> 220.0
                     _ -> 0.0
                   
                   s = evaluateState weights (turn st) nextSt
               in s + blunderPen + lancePen + checkBonus + tacticalBonus"""

# 順次置換
norm_content = content.replace('\r\n', '\n')

def do_replace(src, tgt, rep, name):
    norm_tgt = tgt.replace('\r\n', '\n')
    norm_rep = rep.replace('\r\n', '\n')
    if norm_tgt in src:
        src = src.replace(norm_tgt, norm_rep)
        print(f"SUCCESS: {name}")
    else:
        print(f"FAILED: {name}")
    return src

norm_content = do_replace(norm_content, target_weights, new_weights, "MidgameWeights")

# evalNumericalAssault の挿入（isFriendlySquare の直前）
marker = "isFriendlySquare :: Square -> Player -> Board -> Bool"
if marker in norm_content:
    norm_content = norm_content.replace(marker, eval_assault_code + "\n" + marker)
    print("SUCCESS: evalNumericalAssault insertion")
else:
    print("FAILED: evalNumericalAssault marker not found")

norm_content = do_replace(norm_content, old_blunder, new_blunder, "isBlunderMove")
norm_content = do_replace(norm_content, old_eval, new_eval, "evaluateState")
norm_content = do_replace(norm_content, old_qs, new_qs, "searchQuiescence")
norm_content = do_replace(norm_content, old_scoremove, new_scoremove, "scoreMove")

with open(r'c:\VS\Workspace\t-shogi\ShogiUsiMorse-v4.hs', 'w', encoding='utf-8', newline='\n') as f:
    f.write(norm_content)

print("All updates processed.")
