# Script to enhance ShogiUsiMorse-v4.hs with:
# 1. 4-piece iron defense (金銀桂香) in evalMidgameDefense
# 2. 1-tempo win (一手勝ち) and iron defense overwhelming in searchAlphaBeta

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

# --- Step 1: Update evalMidgameDefense ---
found_def = False
for i, l in enumerate(lines):
    if 'evalMidgameDefense :: Player -> State -> Double' in l:
        found_def = True
        # Find where "castleBonus, dangerAroundKing" is defined
        for j in range(i, i + 35):
            if 'guardBonus = fromIntegral guardCount * 45.0' in lines[j]:
                # Locate start of myGuards
                start_guards = None
                for k in range(i, j):
                    if 'let myGuards = [' in lines[k]:
                        start_guards = k
                        break
                assert start_guards is not None, "myGuards start not found"
                
                # Replace guards calculation up to enemyAttacks line
                end_guards = None
                for k in range(j, j + 15):
                    if 'in (guardBonus + connectedBonus, enemyAttacks)' in lines[k]:
                        end_guards = k
                        break
                assert end_guards is not None, "end_guards not found"
                
                new_defense_lines = [
                    "          let -- 自玉から距離3以内、または自陣深くに位置する守備駒（金・銀・桂・香）\n",
                    "              myGuards = [ (sq, pt) | (sq, Piece p pt) <- Map.toList bd\n",
                    "                                    , p == plr\n",
                    "                                    , pt `elem` [Gold, Silver, PPawn, PSilver, Knight, Lance]\n",
                    "                                    , abs (fst sq - kx) <= 3 && abs (snd sq - ky) <= 3 ]\n",
                    "              guardCount = length myGuards\n",
                    "              \n",
                    "              -- ★ 守りの4枚（金、銀、桂、香）の配備状況\n",
                    "              hasGuardGold   = any (\\(_, pt) -> pt `elem` [Gold, PPawn, PSilver]) myGuards\n",
                    "              hasGuardSilver = any (\\(_, pt) -> pt `elem` [Silver, PSilver]) myGuards\n",
                    "              hasGuardKnight = any (\\(_, pt) -> pt == Knight) myGuards\n",
                    "              hasGuardLance  = any (\\(_, pt) -> pt == Lance) myGuards\n",
                    "              \n",
                    "              guardTypesCount = (if hasGuardGold then 1 else 0) +\n",
                    "                                (if hasGuardSilver then 1 else 0) +\n",
                    "                                (if hasGuardKnight then 1 else 0) +\n",
                    "                                (if hasGuardLance then 1 else 0)\n",
                    "                                \n",
                    "              -- 「4枚の攻めを受けきれる守り（金銀桂香）」の特大ボーナス\n",
                    "              fourPieceDefenseBonus = case guardTypesCount of\n",
                    "                4 -> 650.0 -- 金・銀・桂・香が完備された鉄壁陣形！4枚の攻めも完全に受けきる！\n",
                    "                3 -> 350.0 -- 3枚の守り\n",
                    "                2 -> 150.0\n",
                    "                _ -> 0.0\n",
                    "                \n",
                    "              -- 金銀同士・守備駒同士の紐付き（連結度スクラム）\n",
                    "              linkPairs = [ 1 | ((sq1, _), (sq2, _)) <- [(g1, g2) | g1 <- myGuards, g2 <- myGuards, fst g1 < fst g2]\n",
                    "                              , canPieceReachTarget sq1 (Piece plr Gold) sq2 bd || canPieceReachTarget sq1 (Piece plr Silver) sq2 bd ]\n",
                    "              connectedBonus = fromIntegral (sum linkPairs) * 60.0\n",
                    "              guardBonus = fromIntegral guardCount * 40.0\n",
                    "              \n",
                    "              -- 自玉周辺への敵利きの侵入ペナルティ\n",
                    "              kingNeighbors = [ (kx + dx, ky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0), not (isOutOfBounds (kx + dx, ky + dy)) ]\n",
                    "              enemyAttacks = sum [ if isCoveredBy opp sq bd then 50.0 else 0.0 | sq <- kingNeighbors ]\n",
                    "          in (guardBonus + connectedBonus + fourPieceDefenseBonus, enemyAttacks)\n"
                ]
                lines = lines[:start_guards] + new_defense_lines + lines[end_guards+1:]
                print("evalMidgameDefense updated with 4-piece iron defense!")
                break
        break
assert found_def, "evalMidgameDefense not found"

# --- Step 2: Update searchAlphaBeta to add 1-tempo win & ironclad defense bonus ---
found_sum = False
for i, l in enumerate(lines):
    if 's = evaluateState weights (turn st) nextSt' in l:
        found_sum = True
        # Insert 1-tempo win bonus calculation right before s = evaluateState
        indent = "                    "
        win_calc = [
            indent + "-- ★ 1手勝ち（速度勝ち: Speed Race Advantage）判定\n",
            indent + "-- 自玉が安全（王手・即詰みがない）状態で、敵玉に詰めろをかける、または1手差で迫る手筋\n",
            indent + "myKingSqNow = findKing (turn st) nextBd\n",
            indent + "isMyKingSafe = not (isKingUnderAttack (turn st) nextBd) &&\n",
            indent + "               (case myKingSqNow of\n",
            indent + "                  Just (mkx, mky) ->\n",
            indent + "                    let myNeighbors = [ (mkx + dx, mky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0), not (isOutOfBounds (mkx + dx, mky + dy)) ]\n",
            indent + "                        oppPressures = length [ sq | sq <- myNeighbors, isCoveredBy opp sq nextBd ]\n",
            indent + "                    in oppPressures <= 2\n",
            indent + "                  Nothing -> False)\n",
            indent + "oppKingSqNext = findKing opp nextBd\n",
            indent + "isPressuringOppKing = case oppKingSqNext of\n",
            indent + "  Just (okx, oky) -> abs (fst toSq - okx) <= 2 && abs (snd toSq - oky) <= 2\n",
            indent + "  Nothing         -> False\n",
            indent + "oneTempoWinBonus = if isMyKingSafe && (isCheck || isPressuringOppKing)\n",
            indent + "                   then 1200.0 -- 1手勝ち確定！自玉安全で敵玉に迫る！\n",
            indent + "                   else 0.0\n",
            indent + "\n",
            indent + "-- ★ 金銀桂香による鉄壁受け（4枚の攻めを受け止める合駒・引き受け・底歩・底香）\n",
            indent + "isDefendingMyKing = case myKingSqNow of\n",
            indent + "  Just (mkx, mky) -> abs (fst toSq - mkx) <= 2 && abs (snd toSq - mky) <= 2 && ptMoved `elem` [Gold, Silver, Knight, Lance, Pawn]\n",
            indent + "  Nothing         -> False\n",
            indent + "ironcladDefenseBonus = if isDefendingMyKing && not (null oppAttackers) && not (null friendlyDefenders)\n",
            indent + "                       then 800.0 -- 敵の攻めを金銀桂香でガッチリ受け止めた！\n",
            indent + "                       else 0.0\n",
            indent + "\n"
        ]
        lines = lines[:i] + win_calc + lines[i:]
        
        # Now find the summation line and add oneTempoWinBonus + ironcladDefenseBonus
        for k in range(i + len(win_calc), i + len(win_calc) + 10):
            if 'in s + blunderPen + lancePen + suicidePen' in lines[k]:
                lines[k] = lines[k].rstrip() + " + oneTempoWinBonus + ironcladDefenseBonus\n"
                print("Summation updated with oneTempoWinBonus and ironcladDefenseBonus!")
                break
        break

assert found_sum, "Summation in searchAlphaBeta not found"

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)
print("ShogiUsiMorse-v4.hs successfully updated!")
