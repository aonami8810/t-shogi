# Update GenerateStratifiedCompleteBook.hs scoreM
with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

found_scoreM = False
for i, l in enumerate(lines):
    if 'let scoreM m =' in l:
        found_scoreM = True
        # Find where scored = sortBy ... starts
        end_idx = None
        for j in range(i, i + 25):
            if 'scored = sortBy' in lines[j]:
                end_idx = j
                break
        assert end_idx is not None, "scored = sortBy not found"
        
        indent = lines[i][:lines[i].index('let scoreM')]
        pad = indent + "    "
        new_scoreM = [
            indent + "let scoreM m =\n",
            pad + "let plr = turn s\n",
            pad + "    opp = opponent plr\n",
            pad + "    (toSq, ptMoved, isPromo) = case m of\n",
            pad + "      NormalMove from t p ->\n",
            pad + "        case Map.lookup from (board s) of\n",
            pad + "          Just (Piece _ pt) -> (t, pt, p)\n",
            pad + "          Nothing           -> (t, Pawn, p)\n",
            pad + "      DropMove pt t -> (t, pt, False)\n",
            pad + "    nextS = applyMove m s\n",
            pad + "    nextBd = board nextS\n",
            pad + "    isMajor = ptMoved `elem` [Rook, PRook, Bishop, PBishop]\n",
            pad + "    oppAttackers = getPiecesCovering opp toSq nextBd\n",
            pad + "    friendlyDefenders = getPiecesCovering plr toSq nextBd\n",
            pad + "    targetCaptured = Map.lookup toSq (board s)\n",
            pad + "    isCapturingValuable = case targetCaptured of\n",
            pad + "      Just (Piece _ capPt) -> pieceValue capPt >= 350.0\n",
            pad + "      Nothing              -> False\n",
            pad + "    hasCheapAttacker = any (\\(_, Piece _ opt) -> opt `elem` [Pawn, Lance, Knight, Silver]) oppAttackers\n",
            pad + "    \n",
            pad + "    -- 1. 飛車・角のタダ捨て・無謀な成り捨て・大損交換への特大ペナルティ\n",
            pad + "    isMajorSuicide = isMajor && not (null oppAttackers) && null friendlyDefenders && not isCapturingValuable\n",
            pad + "    isMajorCheapTrade = isMajor && hasCheapAttacker && not isCapturingValuable\n",
            pad + "    suicidePen = if isMajorSuicide then -15000.0 else if isMajorCheapTrade then -10000.0 else 0.0\n",
            pad + "    \n",
            pad + "    -- 2. 駒得・成り・王手評価\n",
            pad + "    capVal = case targetCaptured of\n",
            pad + "      Just (Piece _ pt) -> pieceValue pt * 10.0\n",
            pad + "      Nothing           -> 0.0\n",
            pad + "    promoVal = if (isMajorSuicide || isMajorCheapTrade) then 0.0 else (if isPromo then 500.0 else 0.0)\n",
            pad + "    checkVal = if (isMajorSuicide || isMajorCheapTrade) then 0.0 else (if isKingUnderAttack opp nextBd then 800.0 else 0.0)\n",
            pad + "    \n",
            pad + "    -- 3. ★ 4枚の攻め（銀・桂の前進・援護）への積極加点\n",
            pad + "    isFourPieceAdvance = ptMoved `elem` [Silver, Knight] && (if plr == Sente then snd toSq <= 5 else snd toSq >= 5)\n",
            pad + "    fourPieceBonus = if isFourPieceAdvance then 350.0 else 0.0\n",
            pad + "in capVal + promoVal + checkVal + suicidePen + fourPieceBonus\n"
        ]
        lines = lines[:i] + new_scoreM + lines[end_idx:]
        print("scoreM updated in GenerateStratifiedCompleteBook.hs!")
        break

assert found_scoreM, "let scoreM m not found"

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)
print("Saved GenerateStratifiedCompleteBook.hs successfully!")
