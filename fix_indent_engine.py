# Fix indentation in ShogiUsiMorse-v4.hs
with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

# Inspect lines around 1520 to see the exact indentation of let bindings
indent = "                   " # 19 spaces
for i in range(1515, 1530):
    if 'lancePen =' in lines[i]:
        indent = lines[i][:lines[i].index('lancePen')]
        print(f"Detected indent: {repr(indent)} ({len(indent)} spaces)")
        break

# Re-read and re-apply step 3 with correct indent
target_start = None
for i in range(1510, 1535):
    if '(toSq, ptMoved, isPromo) = case m of' in lines[i]:
        target_start = i - 1
        break
assert target_start is not None, "Target start not found"

target_end = None
for j in range(target_start, target_start + 150):
    if 'antiQuotientSafetyBonus = evalAntiQuotientSafety st m' in lines[j]:
        target_end = j
        break
assert target_end is not None, "Target end not found"

pad = indent
new_chunk = [
    pad + "-- ★ 大駒のタダ捨て・無謀な成り捨て・大損交換の完全検知と強烈ペナルティ排除（王手以外もすべて対象！）\n",
    pad + "(toSq, ptMoved, isPromo) = case m of\n",
    pad + "  NormalMove _ t p ->\n",
    pad + "    case Map.lookup t nextBd of\n",
    pad + "      Just (Piece _ pt) -> (t, pt, p)\n",
    pad + "      Nothing           -> (t, Pawn, False)\n",
    pad + "  DropMove pt t    -> (t, pt, False)\n",
    pad + "isMajor = ptMoved `elem` [Rook, PRook, Bishop, PBishop]\n",
    pad + "oppAttackers = getPiecesCovering opp toSq nextBd\n",
    pad + "friendlyDefenders = getPiecesCovering (turn st) toSq nextBd\n",
    pad + "targetCaptured = Map.lookup toSq (board st)\n",
    pad + "isCapturingValuable = case targetCaptured of\n",
    pad + "  Just (Piece _ capPt) -> pieceValue capPt >= 350.0\n",
    pad + "  Nothing              -> False\n",
    pad + "\n",
    pad + "-- 1. 紐なしで相手の利きのマスに大駒を突入・打ち込む手（タダ捨て・タダ成り捨て）\n",
    pad + "isMajorSuicide = isMajor && not (null oppAttackers) && null friendlyDefenders && not isCapturingValuable\n",
    pad + "-- 2. 相手の歩・香・桂・銀などの安い駒で大駒を取られる大損交換成り捨て・打ち込み\n",
    pad + "hasCheapAttacker = any (\\(_, Piece _ opt) -> opt `elem` [Pawn, Lance, Knight, Silver]) oppAttackers\n",
    pad + "isMajorCheapTrade = isMajor && hasCheapAttacker && not isCapturingValuable\n",
    pad + "\n",
    pad + "-- 大駒のタダ捨て・成り捨て・大損交換なら超特大ペナルティを科す！\n",
    pad + "suicidePen = if isMajorSuicide\n",
    pad + "             then -15000.0 -- 飛車・角のタダ捨て・タダ成り捨ては絶対に禁止！\n",
    pad + "             else if isMajorCheapTrade\n",
    pad + "             then -10000.0 -- 安い駒にタダ取りされる大損交換も絶対に禁止！\n",
    pad + "             else 0.0\n",
    pad + "\n",
    pad + "checkBonus = if isMajorSuicide || isMajorCheapTrade\n",
    pad + "             then 0.0\n",
    pad + "             else if isCheck then 400.0 else 0.0\n",
    pad + "forkBonus = if isMajorSuicide || isMajorCheapTrade then 0.0 else (if isForkMove st m then 450.0 else 0.0)\n",
    pad + "\n",
    pad + "-- ★ 4枚の攻め（銀・桂の前進・援護）への積極加点\n",
    pad + "-- 飛車・角の攻めに銀や桂が前進して援護する手、または敵玉周辺への前進\n",
    pad + "isFourPieceAdvance = ptMoved `elem` [Silver, Knight] && (if turn st == Sente then snd toSq <= 5 else snd toSq >= 5)\n",
    pad + "fourPieceAdvanceBonus = if isFourPieceAdvance then 350.0 else 0.0\n",
    pad + "\n",
    pad + "-- ★ QEGTB（商空間エンドゲームテーブル）の配置空間と同値になる手への優先ボーナス！\n",
    pad + "quotientEquivBonus = evalQuotientEquivalence st m\n",
    pad + "\n",
    pad + "-- ★ とびごま（飛、竜、角、馬、桂、香）の潜在的な利きを活かした戦術ボーナス\n",
    pad + "fromSq = case m of\n",
    pad + "  NormalMove f _ _ -> Just f\n",
    pad + "  DropMove _ _     -> Nothing\n",
    pad + "\n",
    pad + "-- 1. 開き王手（Discovered Check）：自駒を動かしたマス自身ではなく、背後の飛び駒（飛・角・香）から敵玉に王手がかかった手！\n",
    pad + "isDiscoveredCheck = isCheck && case fromSq of\n",
    pad + "  Just _ ->\n",
    pad + "    let oppK = findKing opp nextBd\n",
    pad + "        movedPiece = Map.lookup toSq nextBd\n",
    pad + "    in case (oppK, movedPiece) of\n",
    pad + "         (Just kSq, Just p) -> not (canPieceReachTarget toSq p kSq nextBd)\n",
    pad + "         _                  -> False\n",
    pad + "  Nothing -> False\n",
    pad + "discoveredBonus = if isDiscoveredCheck then 600.0 else 0.0\n",
    pad + "\n",
    pad + "-- 2. 潜在的射線の開放（Unblocking Ray）：駒を動かしたことで、背後の飛・角・香の射線が開通した\n",
    pad + "rayUnblockBonus = case fromSq of\n",
    pad + "  Just f ->\n",
    pad + "    let behindPieces = [ bsq | (bsq, Piece bp bpt) <- Map.toList (board st)\n",
    pad + "                             , bp == turn st, bpt `elem` [Rook, PRook, Bishop, PBishop, Lance]\n",
    pad + "                             , canPieceXRayTarget bsq (Piece bp bpt) f (board st) ]\n",
    pad + "    in if not (null behindPieces) then 280.0 else 0.0\n",
    pad + "  Nothing -> 0.0\n",
    pad + "\n",
    pad + "-- 3. 飛び駒を敵玉と同じライン（筋・段・斜め）に据える手、または桂馬の急襲\n",
    pad + "potentialAlignBonus = case m of\n",
    pad + "  NormalMove _ to _ ->\n",
    pad + "    if ptMoved `elem` [Rook, PRook, Bishop, PBishop, Lance, Knight]\n",
    pad + "    then case findKing opp (board st) of\n",
    pad + "           Just (ekx, eky) ->\n",
    pad + "             let (tx, ty) = to\n",
    pad + "                 isAligned = (ptMoved `elem` [Rook, PRook, Lance] && tx == ekx) ||\n",
    pad + "                             (ptMoved `elem` [Rook, PRook] && ty == eky) ||\n",
    pad + "                             (ptMoved `elem` [Bishop, PBishop] && abs (tx - ekx) == abs (ty - eky)) ||\n",
    pad + "                             (ptMoved == Knight && abs (tx - ekx) <= 2 && abs (ty - eky) <= 2)\n",
    pad + "             in if isAligned then 180.0 else 0.0\n",
    pad + "           Nothing -> 0.0\n",
    pad + "    else 0.0\n",
    pad + "  DropMove pt to ->\n",
    pad + "    case findKing opp (board st) of\n",
    pad + "      Just (ekx, eky) ->\n",
    pad + "        let (tx, ty) = to\n",
    pad + "            isAligned = (pt `elem` [Rook, Lance] && tx == ekx) ||\n",
    pad + "                        (pt == Rook && ty == eky) ||\n",
    pad + "                        (pt == Bishop && abs (tx - ekx) == abs (ty - eky)) ||\n",
    pad + "                        (pt == Knight && abs (tx - ekx) <= 2 && abs (ty - eky) <= 2)\n",
    pad + "        in if isAligned then 220.0 else 0.0\n",
    pad + "      Nothing -> 0.0\n",
    pad + "\n",
    pad + "-- 大駒の成り捨て・重要駒破壊・打ち込み・数の攻めボーナス\n",
    pad + "tacticalBonus = if isMajorSuicide || isMajorCheapTrade then 0.0 else case m of\n",
    pad + "  -- 大駒の成り（安全な成り、または大駒成り）\n",
    pad + "  NormalMove _ _ True -> if isCheck then 500.0 else 240.0\n",
    pad + "  -- 敵玉近傍への大駒・金銀の打ち込み（王手ならさらに加点）\n",
    pad + "  DropMove pt to ->\n",
    pad + "    let baseDrop = case findKing opp (board st) of\n",
    pad + "          Just (kx, ky) | abs (fst to - kx) <= 2 && abs (snd to - ky) <= 2 ->\n",
    pad + "            if pt `elem` [Gold, Silver, Rook, Bishop, Knight] then 320.0 else 120.0\n",
    pad + "          _ -> 60.0\n",
    pad + "    in if isCheck then baseDrop + 200.0 else baseDrop\n",
    pad + "  -- 飛車先歩突き・角道オープン\n",
    pad + "  NormalMove (2, 7) (2, 6) _ | turn st == Sente -> 280.0\n",
    pad + "  NormalMove (2, 6) (2, 5) _ | turn st == Sente -> 240.0\n",
    pad + "  NormalMove (8, 3) (8, 4) _ | turn st == Gote  -> 280.0\n",
    pad + "  NormalMove (8, 4) (8, 5) _ | turn st == Gote  -> 240.0\n",
    pad + "  NormalMove (7, 7) (7, 6) _ | turn st == Sente -> 220.0\n",
    pad + "  NormalMove (3, 3) (3, 4) _ | turn st == Gote  -> 220.0\n",
    pad + "  _ -> 0.0\n",
    pad + "\n"
]

lines = lines[:target_start] + new_chunk + lines[target_end:]
print("Applied with exact indentation!")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)
print("Saved!")
