with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

def replace_block(src, start_str, end_str, new_code):
    s_idx = src.find(start_str)
    if s_idx == -1:
        raise ValueError(f"Start string not found: {start_str}")
    e_idx = src.find(end_str, s_idx)
    if e_idx == -1:
        raise ValueError(f"End string not found: {end_str}")
    return src[:s_idx] + new_code + src[e_idx + len(end_str):]

# 1. Fix wanderingGoldPen: prohibit ANY backward retreat (ty > fy for Sente) and returning to rank 9/1
old_gold_pen = """          wanderingGoldPen = case m of
            NormalMove from to _ ->
              case Map.lookup from curBd of
                Just (Piece p Gold) | p == plr ->
                  let curKing = findKing plr curBd
                      oppKing = findKing opp curBd
                      isCapturing = Map.member to curBd
                      isNearOppKing = case oppKing of
                        Just (ekx, eky) -> max (abs (fst to - ekx)) (abs (snd to - eky)) <= 2
                        Nothing         -> False
                      isHomeSquare = if plr == Sente
                                     then to == (4, 9) || to == (6, 9)
                                     else to == (6, 1) || to == (4, 1)
                      homePen = if isHomeSquare then -5000.0 else 0.0
                  in if isCapturing || isNearOppKing
                     then 0.0
                     else
                       let distCur = case curKing of Just (kx, ky) -> abs (fst from - kx) + abs (snd from - ky); Nothing -> 0
                           distNext = case curKing of Just (kx, ky) -> abs (fst to - kx) + abs (snd to - ky); Nothing -> 0
                           movingAwayPen = if distNext > distCur && distNext > 2 then -600.0 else 0.0
                           isMidRank = snd to == 5 || (plr == Sente && snd to == 6) || (plr == Gote && snd to == 4)
                           midRankPen = if isMidRank then -500.0 else 0.0
                       in homePen + movingAwayPen + midRankPen
                _ -> 0.0
            _ -> 0.0"""

new_gold_pen = """          -- 3. 金の無駄戻り（Gold Pointless Retreat / Backstepping）の完全根絶
          wanderingGoldPen = case m of
            NormalMove from to _ ->
              case Map.lookup from curBd of
                Just (Piece p Gold) | p == plr ->
                  let curKing = findKing plr curBd
                      oppKing = findKing opp curBd
                      isCapturing = Map.member to curBd
                      inCheck = isKingUnderAttack plr curBd
                      (fx, fy) = from
                      (tx, ty) = to
                      isNearOppKing = case oppKing of
                        Just (ekx, eky) -> max (abs (tx - ekx)) (abs (ty - eky)) <= 2
                        Nothing         -> False
                      -- 金の後退（退行手）：先手なら下方向(ty > fy)、後手なら上方向(ty < fy)
                      isRetreat = if plr == Sente then ty > fy else ty < fy
                      -- 1段目（初期配置の底段 9段目 / 1段目）に戻る大悪手
                      isRank1Or9 = if plr == Sente then ty == 9 else ty == 1
                      isHomeSquare = if plr == Sente
                                     then to `elem` [(4, 9), (5, 9), (6, 9)]
                                     else to `elem` [(6, 1), (5, 1), (4, 1)]
                      retreatPen = if isRetreat && not isCapturing && not inCheck
                                   then (if isHomeSquare || isRank1Or9 then -6000.0 else -4000.0)
                                   else 0.0
                  in if isCapturing || isNearOppKing || inCheck
                     then 0.0
                     else
                       let distCur = case curKing of Just (kx, ky) -> abs (fx - kx) + abs (fy - ky); Nothing -> 0
                           distNext = case curKing of Just (kx, ky) -> abs (tx - kx) + abs (ty - ky); Nothing -> 0
                           movingAwayPen = if distNext > distCur && distNext > 2 then -800.0 else 0.0
                       in retreatPen + movingAwayPen
                _ -> 0.0
            _ -> 0.0"""

content = content.replace(old_gold_pen, new_gold_pen)
print("1. Replaced wanderingGoldPen with strict anti-retreat logic.")

# 2. Fix aimlessRookPen: remove king-distance loophole, penalize ANY re-swinging / pointless file switch
old_aimless_rook = """         -- 4. 飛車のうろうろ（Aimless Rook Wandering）検知：
-- 4. 飛車のうろうろ・無駄な手損・目的のない筋変更の徹底抑止
         aimlessRookPen = case m of
           NormalMove from to promo ->
             case Map.lookup from curBd of
               Just (Piece p Rook) | p == plr ->
                 let isCapturing = Map.member to curBd
                     isCheck = isKingUnderAttack opp nextBd
                     isPromo = promo
                     (fx, fy) = from
                     (tx, ty) = to
                     -- 石田流の正当な手筋（7八飛、7六浮き飛車、3二飛、3四浮き飛車）は手損ではない！
                     isIshidaLegit = (plr == Sente && (to == (7, 8) || (from == (7, 8) && to == (7, 6)))) ||
                                     (plr == Gote && (to == (3, 2) || (from == (3, 2) && to == (3, 4))))
                     -- 飛車先が歩で塞がれているのに自陣をふらふら横移動する手
                     isPointlessFileSwitch =
                       not isIshidaLegit && fx /= tx &&
                       null (getPiecesCovering opp from curBd) &&
                       (case findKing opp curBd of
                          Just (ekx, _) -> abs (tx - ekx) >= abs (fx - ekx)
                          Nothing       -> True)
                     -- 目的のない自陣内の縦往復（ただ下がる）
                     isPointlessVertical =
                       not isIshidaLegit && fx == tx &&
                       null (getPiecesCovering opp from curBd) &&
                       (plr == Sente && ty > fy || plr == Gote && ty < fy)
                 in if isCapturing || isCheck || isPromo || isIshidaLegit
                    then 0.0
                    else if isPointlessFileSwitch
                    then -3500.0 -- 飛車の無駄な筋変更・うろうろ横移動に厳罰！
                    else if isPointlessVertical
                    then -2500.0 -- 飛車の無駄な自陣後退・手損に厳罰！
                    else -1200.0 -- 目的の薄い飛車の移動手損
               _ -> 0.0
           _ -> 0.0"""

new_aimless_rook = """         -- 4. 飛車の振り直し・筋変更・うろうろ手損の完全根絶（Anti-Reswinging & Anti-Wandering）
         aimlessRookPen = case m of
           NormalMove from to promo ->
             case Map.lookup from curBd of
               Just (Piece p Rook) | p == plr ->
                 let isCapturing = Map.member to curBd
                     isCheck = isKingUnderAttack opp nextBd
                     isPromo = promo
                     (fx, fy) = from
                     (tx, ty) = to
                     -- 正当な振飛車・浮き飛車手筋：
                     -- 石田流: 7八飛, 7六浮き飛車 (後手 3二飛, 3四浮き飛車)
                     -- 中飛車: 5八飛, 5六浮き飛車 (後手 5二飛, 5四浮き飛車)
                     -- 四間飛車: 6八飛 (後手 4二飛)
                     -- 居飛車浮き飛車: 2八飛から2六飛
                     isLegitOpeningRook =
                       (plr == Sente && ((from == (2, 8) && to `elem` [(7, 8), (5, 8), (6, 8), (2, 6)]) ||
                                         (from == (7, 8) && to == (7, 6)) ||
                                         (from == (5, 8) && to == (5, 6)))) ||
                       (plr == Gote &&  ((from == (8, 2) && to `elem` [(3, 2), (5, 2), (4, 2), (8, 4)]) ||
                                         (from == (3, 2) && to == (3, 4)) ||
                                         (from == (5, 2) && to == (5, 4))))
                     
                     -- 振り直し（Re-swinging）：一度初期位置(2八/8二)から離れた飛車が、別の筋に再度横移動する手損
                     isReSwinging = (plr == Sente && from /= (2, 8) && fx /= tx && not (from == (7, 8) && to == (7, 6)) && not (from == (5, 8) && to == (5, 6))) ||
                                    (plr == Gote &&  from /= (8, 2) && fx /= tx && not (from == (3, 2) && to == (3, 4)) && not (from == (5, 2) && to == (5, 4)))
                     
                     -- 飛車の無目的後退（ただ下がる）
                     isPointlessRetreat = fx == tx && (plr == Sente && ty > fy || plr == Gote && ty < fy)
                     -- 目的のない自陣内の横移動
                     isAimlessHorizontal = fx /= tx && not isLegitOpeningRook
                 in if isCapturing || isCheck || isPromo
                    then 0.0
                    else if isReSwinging
                    then -5000.0 -- 一度振った飛車の振り直し・筋変更は超特大ペナルティ！
                    else if isAimlessHorizontal
                    then -4500.0 -- 目的のない飛車の自陣横移動・手損に厳罰！
                    else if isPointlessRetreat
                    then -3500.0 -- 飛車の無駄な自陣後退に厳罰！
                    else if isLegitOpeningRook
                    then 0.0
                    else -1500.0
               _ -> 0.0
           _ -> 0.0"""

content = content.replace(old_aimless_rook, new_aimless_rook)
print("2. Replaced aimlessRookPen with zero-tolerance anti-reswinging.")

# 3. Fix retreatBonus in evalDefensiveTechnique (eliminate +320 bonus for retreating generals)
old_retreat_bonus = """                  -- 初期位置への退行でない健全な引き受けのみボーナス
                  retreatBonus = if isRetreat && isClosingToKing && not isHomeSquare && not isAbandoningCentralKeystone
                                 then 320.0
                                 else 0.0"""

new_retreat_bonus = """                  -- 金銀の後退ボーナスは完全撤廃（金の無駄戻りの元凶を根絶）
                  retreatBonus = 0.0"""

content = content.replace(old_retreat_bonus, new_retreat_bonus)
print("3. Eliminated general retreatBonus in evalDefensiveTechnique.")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("Saved first round of fixes successfully!")
