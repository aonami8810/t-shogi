# Accurate updater for ShogiUsiMorse-v4.hs
import re

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

# 1. Update wanderingGoldPen
found_wandering = False
for i, l in enumerate(lines):
    if 'wanderingGoldPen = case m of' in l:
        found_wandering = True
        # Find where it ends (before aimlessRookPen)
        end_idx = None
        for j in range(i, i + 35):
            if 'aimlessRookPen = case m of' in lines[j]:
                end_idx = j
                break
        assert end_idx is not None, "aimlessRookPen not found"
        # Back up to the comment or blank line before aimlessRookPen
        new_chunk = [
            "         wanderingGoldPen = case m of\n",
            "           NormalMove from to _ ->\n",
            "             case Map.lookup from curBd of\n",
            "               Just (Piece p Gold) | p == plr ->\n",
            "                 let curKing = findKing plr curBd\n",
            "                     oppKing = findKing opp curBd\n",
            "                     isCapturing = Map.member to curBd\n",
            "                     isNearOppKing = case oppKing of\n",
            "                       Just (ekx, eky) -> max (abs (fst to - ekx)) (abs (snd to - eky)) <= 2\n",
            "                       Nothing         -> False\n",
            "                     isHomeSquare = if plr == Sente\n",
            "                                    then to == (4, 9) || to == (6, 9)\n",
            "                                    else to == (6, 1) || to == (4, 1)\n",
            "                     homePen = if isHomeSquare then -5000.0 else 0.0\n",
            "                 in if isCapturing || isNearOppKing\n",
            "                    then 0.0\n",
            "                    else\n",
            "                      let distCur = case curKing of Just (kx, ky) -> abs (fst from - kx) + abs (snd from - ky); Nothing -> 0\n",
            "                          distNext = case curKing of Just (kx, ky) -> abs (fst to - kx) + abs (snd to - ky); Nothing -> 0\n",
            "                          movingAwayPen = if distNext > distCur && distNext > 2 then -600.0 else 0.0\n",
            "                          isMidRank = snd to == 5 || (plr == Sente && snd to == 6) || (plr == Gote && snd to == 4)\n",
            "                          midRankPen = if isMidRank then -500.0 else 0.0\n",
            "                      in homePen + movingAwayPen + midRankPen\n",
            "               _ -> 0.0\n",
            "           _ -> 0.0\n",
            "\n",
            "         -- 4. 飛車のうろうろ（Aimless Rook Wandering）検知：\n"
        ]
        # Check if line before aimlessRookPen has comment
        start_replace = i
        lines = lines[:start_replace] + new_chunk + lines[end_idx:]
        break

assert found_wandering, "wanderingGoldPen not found"
print("wanderingGoldPen updated!")

# 2. Update evalDefensiveTechnique
# Search for "solidDefenseScore = case m of"
found_defensive = False
for i, l in enumerate(lines):
    if 'solidDefenseScore = case m of' in l:
        found_defensive = True
        # Find where evalDefensiveTechnique ends (line containing "in pawnDropDefenseScore + solidDefenseScore")
        end_idx = None
        for j in range(i, i + 100):
            if 'pawnDropDefenseScore + solidDefenseScore' in lines[j]:
                end_idx = j
                break
        assert end_idx is not None, "end of evalDefensiveTechnique not found"

        new_defense_chunk = [
            "      -- 2. 金銀の引き受け・玉との連結（Gold/Silver Solidification）\n",
            "      -- ★ 厳重注意：金が理由なく初期位置（先手4九・6九 / 後手6一・4一）に戻る手（Home Retreat Blunder）は、\n",
            "      --   陣形破壊・コビン開放・角打ち被弾の主因であり、絶対にボーナスを与えず特大ペナルティを科す！\n",
            "      solidDefenseScore = case m of\n",
            "        NormalMove from to _ ->\n",
            "          case Map.lookup from curBd of\n",
            "            Just (Piece p pt) | p == plr && pt `elem` [Gold, Silver] ->\n",
            "              let isRetreat = if plr == Sente then snd to > snd from else snd to < snd from\n",
            "                  distNext = case nextKingSq of\n",
            "                    Just (kx, ky) -> abs (fst to - kx) + abs (snd to - ky)\n",
            "                    Nothing       -> 99\n",
            "                  distCur = case curKingSq of\n",
            "                    Just (kx, ky) -> abs (fst from - kx) + abs (snd from - ky)\n",
            "                    Nothing       -> 99\n",
            "                  isClosingToKing = distNext < distCur || (distNext <= 2 && distCur <= 2)\n",
            "                  \n",
            "                  -- 金の初期位置への戻り判定 (Home Retreat Blunder)\n",
            "                  isGold = pt == Gold\n",
            "                  isHomeSquare = if plr == Sente\n",
            "                                 then to == (4, 9) || to == (6, 9)\n",
            "                                 else to == (6, 1) || to == (4, 1)\n",
            "                  isCapture = Map.member to curBd\n",
            "                  inCheckNow = isKingUnderAttack plr curBd\n",
            "                  \n",
            "                  -- 中飛車陣形における要石（Keystone）の放棄判定\n",
            "                  -- 5八, 4八, 6八, 5七, 4七, 6七 の金が1段目に後退する手を厳禁\n",
            "                  isAbandoningCentralKeystone = \n",
            "                    if plr == Sente\n",
            "                    then isGold && snd from <= 8 && snd to == 9\n",
            "                    else isGold && snd from >= 2 && snd to == 1\n",
            "                    \n",
            "                  homeRetreatPen =\n",
            "                    if isGold && isHomeSquare && not isCapture && not inCheckNow\n",
            "                    then -6000.0 -- 金が元の位置に戻る自滅手には特大ペナルティ！\n",
            "                    else if isAbandoningCentralKeystone && not isCapture && not inCheckNow\n",
            "                    then -5000.0 -- 要石の金を無意味に1段目に引く手損・陣形破壊ペナルティ！\n",
            "                    else 0.0\n",
            "                    \n",
            "                  defendsFriendly = any (\\(sq, Piece own _) -> own == plr && isSquareUnderAttack opp sq curBd && isSquareUnderAttack plr sq nextBd) (Map.toList nextBd)\n",
            "                  defendBonus = if defendsFriendly then 280.0 else 0.0\n",
            "                  -- 初期位置への退行でない健全な引き受けのみボーナス\n",
            "                  retreatBonus = if isRetreat && isClosingToKing && not isHomeSquare && not isAbandoningCentralKeystone\n",
            "                                 then 320.0\n",
            "                                 else 0.0\n",
            "              in homeRetreatPen + retreatBonus + defendBonus\n",
            "            _ -> 0.0\n",
            "        _ -> 0.0\n",
            "        \n",
            "      -- 3. 自陣の角打ち隙（両取り隙）を消す受け\n",
            "      antiForkDefenseScore =\n",
            "        case curKingSq of\n",
            "          Just (kx, ky) ->\n",
            "            let toSq = case m of NormalMove _ t _ -> t; DropMove _ t -> t\n",
            "                isDefendingRear = if plr == Sente then snd toSq >= 8 else snd toSq <= 2\n",
            "            in if isDefendingRear then 180.0 else 0.0\n",
            "          Nothing -> 0.0\n",
            "          \n",
            "      -- 4. ★ 自陣全域（特に中飛車のコビン・玉頭・飛車横）への角打ち・馬作り被弾の完全防止\n",
            "      --    相手が持ち駒に角を持っている場合、自らの手によって角打ちの隙（コビン）を開放した場合は超厳罰（-8000点）！\n",
            "      antiBishopDropAndKobinDefenseScore =\n",
            "        let oppHand = Map.findWithDefault Map.empty opp (hands st)\n",
            "            hasOppBishopInHand = Map.findWithDefault 0 Bishop oppHand > 0\n",
            "            \n",
            "            myCampY = if plr == Sente then [7, 8, 9] else [1, 2, 3]\n",
            "            campSquares = [ (x, y) | x <- [1..9], y <- myCampY ]\n",
            "            \n",
            "            isDangerousBishopDrop bd sq =\n",
            "              not (Map.member sq bd) &&\n",
            "              let myDefenders = getPiecesCovering plr sq bd\n",
            "                  isProtected = not (null myDefenders)\n",
            "                  \n",
            "                  isNearMyKing = case (if plr == Sente then nextKingSq else curKingSq) of\n",
            "                    Just (kx, ky) -> max (abs (fst sq - kx)) (abs (snd sq - ky)) <= 2\n",
            "                    Nothing       -> False\n",
            "                    \n",
            "                  isCentralKeystone = if plr == Sente\n",
            "                                      then sq `elem` [(3,9), (4,9), (5,8), (6,9), (7,9), (4,8), (6,8), (5,7)]\n",
            "                                      else sq `elem` [(7,1), (6,1), (5,2), (4,1), (3,1), (6,2), (4,2), (5,3)]\n",
            "                                      \n",
            "                  testBdWithBishop = Map.insert sq (Piece opp Bishop) bd\n",
            "                  threatenedSquares = [ tsq | (tsq, Piece p pt) <- Map.toList testBdWithBishop\n",
            "                                            , p == plr\n",
            "                                            , pt `elem` [King, Rook, PRook, Gold, Silver]\n",
            "                                            , canPieceReachTarget sq (Piece opp Bishop) tsq testBdWithBishop ]\n",
            "                  isForkingHighValue = length threatenedSquares >= 2 || any (\\tsq -> Map.lookup tsq bd == Just (Piece plr King)) threatenedSquares\n",
            "              in not isProtected && (isNearMyKing || isCentralKeystone || isForkingHighValue)\n",
            "\n",
            "            curDangerousCount = length (filter (isDangerousBishopDrop curBd) campSquares)\n",
            "            nextDangerousCount = length (filter (isDangerousBishopDrop nextBd) campSquares)\n",
            "            \n",
            "            kobinDiff = nextDangerousCount - curDangerousCount\n",
            "            kobinScore =\n",
            "              if kobinDiff > 0\n",
            "              then -8000.0 * fromIntegral kobinDiff\n",
            "              else if kobinDiff < 0\n",
            "              then 2200.0 * fromIntegral (- kobinDiff)\n",
            "              else 0.0\n",
            "              \n",
            "            rookRetreatBonus = case m of\n",
            "              NormalMove from to _ ->\n",
            "                case Map.lookup from curBd of\n",
            "                  Just (Piece p Rook) | p == plr ->\n",
            "                    let isUnderAttack = isSquareUnderAttack opp from curBd\n",
            "                        isRetreatToHome = if plr == Sente then to == (2, 8) else to == (8, 2)\n",
            "                    in if isUnderAttack && isRetreatToHome then 1200.0\n",
            "                       else if isRetreatToHome then 600.0\n",
            "                       else 0.0\n",
            "                  _ -> 0.0\n",
            "              _ -> 0.0\n",
            "        in if hasOppBishopInHand\n",
            "           then kobinScore + rookRetreatBonus\n",
            "           else rookRetreatBonus\n",
            "          \n",
            "  in pawnDropDefenseScore + solidDefenseScore + antiForkDefenseScore + antiBishopDropAndKobinDefenseScore\n"
        ]
        # Find start of comment "2. 金銀の引き受け"
        start_rep = i - 1
        lines = lines[:start_rep] + new_defense_chunk + lines[end_idx+1:]
        break

assert found_defensive, "solidDefenseScore not found"
print("evalDefensiveTechnique updated!")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)
print("ShogiUsiMorse-v4.hs successfully updated and saved!")
