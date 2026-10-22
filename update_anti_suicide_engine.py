# Direct line replacement for ShogiUsiMorse-v4.hs
with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

# Step 3: searchAlphaBeta update
# Find the exact line index around line 1528 where "(toSq, ptMoved, isPromo) = case m of" is inside searchAlphaBeta
target_start = None
for i in range(1500, 1600):
    if '(toSq, ptMoved, isPromo) = case m of' in lines[i]:
        target_start = i - 1 # include the comment line before it
        break
assert target_start is not None, "Target start around 1528 not found"

target_end = None
for j in range(target_start, target_start + 150):
    if 'antiQuotientSafetyBonus = evalAntiQuotientSafety st m' in lines[j]:
        target_end = j
        break
assert target_end is not None, "Target end not found"

new_ab_chunk = [
    "                    -- ★ 大駒のタダ捨て・無謀な成り捨て・大損交換の完全検知と強烈ペナルティ排除（王手以外もすべて対象！）\n",
    "                    (toSq, ptMoved, isPromo) = case m of\n",
    "                      NormalMove _ t p ->\n",
    "                        case Map.lookup t nextBd of\n",
    "                          Just (Piece _ pt) -> (t, pt, p)\n",
    "                          Nothing           -> (t, Pawn, False)\n",
    "                      DropMove pt t    -> (t, pt, False)\n",
    "                    isMajor = ptMoved `elem` [Rook, PRook, Bishop, PBishop]\n",
    "                    oppAttackers = getPiecesCovering opp toSq nextBd\n",
    "                    friendlyDefenders = getPiecesCovering (turn st) toSq nextBd\n",
    "                    targetCaptured = Map.lookup toSq (board st)\n",
    "                    isCapturingValuable = case targetCaptured of\n",
    "                      Just (Piece _ capPt) -> pieceValue capPt >= 350.0\n",
    "                      Nothing              -> False\n",
    "                    \n",
    "                    -- 1. 紐なしで相手の利きのマスに大駒を突入・打ち込む手（タダ捨て・タダ成り捨て）\n",
    "                    isMajorSuicide = isMajor && not (null oppAttackers) && null friendlyDefenders && not isCapturingValuable\n",
    "                    -- 2. 相手の歩・香・桂・銀などの安い駒で大駒を取られる大損交換成り捨て・打ち込み\n",
    "                    hasCheapAttacker = any (\\(_, Piece _ opt) -> opt `elem` [Pawn, Lance, Knight, Silver]) oppAttackers\n",
    "                    isMajorCheapTrade = isMajor && hasCheapAttacker && not isCapturingValuable\n",
    "                    \n",
    "                    -- 大駒のタダ捨て・成り捨て・大損交換なら超特大ペナルティを科す！\n",
    "                    suicidePen = if isMajorSuicide\n",
    "                                 then -15000.0 -- 飛車・角のタダ捨て・タダ成り捨ては絶対に禁止！\n",
    "                                 else if isMajorCheapTrade\n",
    "                                 then -10000.0 -- 安い駒にタダ取りされる大損交換も絶対に禁止！\n",
    "                                 else 0.0\n",
    "                    \n",
    "                    checkBonus = if isMajorSuicide || isMajorCheapTrade\n",
    "                                 then 0.0\n",
    "                                 else if isCheck then 400.0 else 0.0\n",
    "                    forkBonus = if isMajorSuicide || isMajorCheapTrade then 0.0 else (if isForkMove st m then 450.0 else 0.0)\n",
    "                    \n",
    "                    -- ★ 4枚の攻め（銀・桂の前進・援護）への積極加点\n",
    "                    -- 飛車・角の攻めに銀や桂が前進して援護する手、または敵玉周辺への前進\n",
    "                    isFourPieceAdvance = ptMoved `elem` [Silver, Knight] && (if turn st == Sente then snd toSq <= 5 else snd toSq >= 5)\n",
    "                    fourPieceAdvanceBonus = if isFourPieceAdvance then 350.0 else 0.0\n",
    "                    \n",
    "                    -- ★ QEGTB（商空間エンドゲームテーブル）の配置空間と同値になる手への優先ボーナス！\n",
    "                    quotientEquivBonus = evalQuotientEquivalence st m\n",
    "                    \n",
    "                    -- ★ とびごま（飛、竜、角、馬、桂、香）の潜在的な利きを活かした戦術ボーナス\n",
    "                    fromSq = case m of\n",
    "                      NormalMove f _ _ -> Just f\n",
    "                      DropMove _ _     -> Nothing\n",
    "                    \n",
    "                    -- 1. 開き王手（Discovered Check）：自駒を動かしたマス自身ではなく、背後の飛び駒（飛・角・香）から敵玉に王手がかかった手！\n",
    "                    isDiscoveredCheck = isCheck && case fromSq of\n",
    "                      Just _ ->\n",
    "                        let oppK = findKing opp nextBd\n",
    "                            movedPiece = Map.lookup toSq nextBd\n",
    "                        in case (oppK, movedPiece) of\n",
    "                             (Just kSq, Just p) -> not (canPieceReachTarget toSq p kSq nextBd)\n",
    "                             _                  -> False\n",
    "                      Nothing -> False\n",
    "                    discoveredBonus = if isDiscoveredCheck then 600.0 else 0.0\n",
    "                    \n",
    "                    -- 2. 潜在的射線の開放（Unblocking Ray）：駒を動かしたことで、背後の飛・角・香の射線が開通した\n",
    "                    rayUnblockBonus = case fromSq of\n",
    "                      Just f ->\n",
    "                        let behindPieces = [ bsq | (bsq, Piece bp bpt) <- Map.toList (board st)\n",
    "                                                 , bp == turn st, bpt `elem` [Rook, PRook, Bishop, PBishop, Lance]\n",
    "                                                 , canPieceXRayTarget bsq (Piece bp bpt) f (board st) ]\n",
    "                        in if not (null behindPieces) then 280.0 else 0.0\n",
    "                      Nothing -> 0.0\n",
    "                    \n",
    "                    -- 3. 飛び駒を敵玉と同じライン（筋・段・斜め）に据える手、または桂馬の急襲\n",
    "                    potentialAlignBonus = case m of\n",
    "                      NormalMove _ to _ ->\n",
    "                        if ptMoved `elem` [Rook, PRook, Bishop, PBishop, Lance, Knight]\n",
    "                        then case findKing opp (board st) of\n",
    "                               Just (ekx, eky) ->\n",
    "                                 let (tx, ty) = to\n",
    "                                     isAligned = (ptMoved `elem` [Rook, PRook, Lance] && tx == ekx) ||\n",
    "                                                 (ptMoved `elem` [Rook, PRook] && ty == eky) ||\n",
    "                                                 (ptMoved `elem` [Bishop, PBishop] && abs (tx - ekx) == abs (ty - eky)) ||\n",
    "                                                 (ptMoved == Knight && abs (tx - ekx) <= 2 && abs (ty - eky) <= 2)\n",
    "                                 in if isAligned then 180.0 else 0.0\n",
    "                               Nothing -> 0.0\n",
    "                        else 0.0\n",
    "                      DropMove pt to ->\n",
    "                        case findKing opp (board st) of\n",
    "                          Just (ekx, eky) ->\n",
    "                            let (tx, ty) = to\n",
    "                                isAligned = (pt `elem` [Rook, Lance] && tx == ekx) ||\n",
    "                                            (pt == Rook && ty == eky) ||\n",
    "                                            (pt == Bishop && abs (tx - ekx) == abs (ty - eky)) ||\n",
    "                                            (pt == Knight && abs (tx - ekx) <= 2 && abs (ty - eky) <= 2)\n",
    "                            in if isAligned then 220.0 else 0.0\n",
    "                          Nothing -> 0.0\n",
    "                    \n",
    "                    -- 大駒の成り捨て・重要駒破壊・打ち込み・数の攻めボーナス\n",
    "                    tacticalBonus = if isMajorSuicide || isMajorCheapTrade then 0.0 else case m of\n",
    "                      -- 大駒の成り（安全な成り、または大駒成り）\n",
    "                      NormalMove _ _ True -> if isCheck then 500.0 else 240.0\n",
    "                      -- 敵玉近傍への大駒・金銀の打ち込み（王手ならさらに加点）\n",
    "                      DropMove pt to ->\n",
    "                        let baseDrop = case findKing opp (board st) of\n",
    "                              Just (kx, ky) | abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 ->\n",
    "                                if pt `elem` [Gold, Silver, Rook, Bishop, Knight] then 320.0 else 120.0\n",
    "                              _ -> 60.0\n",
    "                        in if isCheck then baseDrop + 200.0 else baseDrop\n",
    "                      -- 飛車先歩突き・角道オープン\n",
    "                      NormalMove (2, 7) (2, 6) _ | turn st == Sente -> 280.0\n",
    "                      NormalMove (2, 6) (2, 5) _ | turn st == Sente -> 240.0\n",
    "                      NormalMove (8, 3) (8, 4) _ | turn st == Gote  -> 280.0\n",
    "                      NormalMove (8, 4) (8, 5) _ | turn st == Gote  -> 240.0\n",
    "                      NormalMove (7, 7) (7, 6) _ | turn st == Sente -> 220.0\n",
    "                      NormalMove (3, 3) (3, 4) _ | turn st == Gote  -> 220.0\n",
    "                      _ -> 0.0\n",
    "                    \n"
]

lines = lines[:target_start] + new_ab_chunk + lines[target_end:]
print("searchAlphaBeta updated!")

# Step 4: Summation line
for i, l in enumerate(lines):
    if 's = evaluateState weights (turn st) nextSt' in l:
        lines[i+1] = "                in s + blunderPen + lancePen + suicidePen + checkBonus + forkBonus + tacticalBonus + fourPieceAdvanceBonus\n"
        print("Summation line updated!")
        break

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)
print("Saved ShogiUsiMorse-v4.hs successfully!")
