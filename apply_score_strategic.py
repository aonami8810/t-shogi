# Exact line replacement for GenerateStratifiedCompleteBook.hs
with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

# 1. Insert scoreMoveStrategic before searchPunishmentIDS512
for i, l in enumerate(lines):
    if 'searchPunishmentIDS512 :: State -> DynamicQEGTB' in l:
        func_lines = [
            "-- | 戦略的着手評価関数（大駒タダ捨て・無謀な成り捨て厳罰化 ＆ 4枚の攻め連携推進）\n",
            "scoreMoveStrategic :: State -> Move -> Double\n",
            "scoreMoveStrategic s m =\n",
            "  let plr = turn s\n",
            "      opp = opponent plr\n",
            "      (toSq, ptMoved, isPromo) = case m of\n",
            "        NormalMove from t p ->\n",
            "          case Map.lookup from (board s) of\n",
            "            Just (Piece _ pt) -> (t, pt, p)\n",
            "            Nothing           -> (t, Pawn, p)\n",
            "        DropMove pt t -> (t, pt, False)\n",
            "      nextS = applyMove m s\n",
            "      nextBd = board nextS\n",
            "      isMajor = ptMoved `elem` [Rook, PRook, Bishop, PBishop]\n",
            "      oppAttackers = getPiecesCovering opp toSq nextBd\n",
            "      friendlyDefenders = getPiecesCovering plr toSq nextBd\n",
            "      targetCaptured = Map.lookup toSq (board s)\n",
            "      isCapturingValuable = case targetCaptured of\n",
            "        Just (Piece _ capPt) -> pieceValue capPt >= 350.0\n",
            "        Nothing              -> False\n",
            "      hasCheapAttacker = any (\\(_, Piece _ opt) -> opt `elem` [Pawn, Lance, Knight, Silver]) oppAttackers\n",
            "      \n",
            "      -- 1. 飛車・角のタダ捨て・無謀な成り捨て・大損交換への特大ペナルティ\n",
            "      isMajorSuicide = isMajor && not (null oppAttackers) && null friendlyDefenders && not isCapturingValuable\n",
            "      isMajorCheapTrade = isMajor && hasCheapAttacker && not isCapturingValuable\n",
            "      suicidePen = if isMajorSuicide then -15000.0 else if isMajorCheapTrade then -10000.0 else 0.0\n",
            "      \n",
            "      -- 2. 駒得・成り・王手評価\n",
            "      capVal = case targetCaptured of\n",
            "        Just (Piece _ pt) -> pieceValue pt * 10.0\n",
            "        Nothing           -> 0.0\n",
            "      promoVal = if (isMajorSuicide || isMajorCheapTrade) then 0.0 else (if isPromo then 500.0 else 0.0)\n",
            "      checkVal = if (isMajorSuicide || isMajorCheapTrade) then 0.0 else (if isKingUnderAttack opp nextBd then 800.0 else 0.0)\n",
            "      \n",
            "      -- 3. ★ 4枚の攻め（銀・桂の前進・援護）への積極加点\n",
            "      isFourPieceAdvance = ptMoved `elem` [Silver, Knight] && (if plr == Sente then snd toSq <= 5 else snd toSq >= 5)\n",
            "      fourPieceBonus = if isFourPieceAdvance then 350.0 else 0.0\n",
            "  in capVal + promoVal + checkVal + suicidePen + fourPieceBonus\n",
            "\n"
        ]
        lines = lines[:i] + func_lines + lines[i:]
        print("scoreMoveStrategic inserted!")
        break

# 2. Replace scoreM in searchPunishmentIDS512
for i, l in enumerate(lines):
    if 'let scoreM m =' in l and i < 260:
        start_replace = i
        end_replace = None
        for j in range(i, i + 20):
            if 'scoredMoves = sortBy' in lines[j]:
                end_replace = j + 1
                break
        assert end_replace is not None, "scoredMoves not found"
        indent = lines[i][:lines[i].index('let scoreM')]
        new_replace = [
            indent + "scoredMoves = sortBy (\\m1 m2 -> compare (scoreMoveStrategic s m2) (scoreMoveStrategic s m1)) nonLoopMoves\n"
        ]
        lines = lines[:start_replace] + new_replace + lines[end_replace:]
        print("First scoreM replaced with scoreMoveStrategic!")
        break

# 3. Replace scoreM in extendEquilibriumToIdeal
for i, l in enumerate(lines):
    if 'let scoreM m =' in l:
        start_replace = i
        end_replace = None
        for j in range(i, i + 20):
            if 'scored = sortBy' in lines[j]:
                end_replace = j + 1
                break
        assert end_replace is not None, "scored = sortBy not found"
        indent = lines[i][:lines[i].index('let scoreM')]
        new_replace = [
            indent + "scored = sortBy (\\m1 m2 -> compare (scoreMoveStrategic s m2) (scoreMoveStrategic s m1)) legalMoves\n"
        ]
        lines = lines[:start_replace] + new_replace + lines[end_replace:]
        print("Second scoreM replaced with scoreMoveStrategic!")
        break

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)
print("GenerateStratifiedCompleteBook.hs updated and saved successfully!")
