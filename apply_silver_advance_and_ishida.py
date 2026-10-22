with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

def find_line(pat, start=0):
    for i in range(start, len(lines)):
        if pat in lines[i]:
            return i
    return -1

# 1. Insert evalSilverAdvanceAndIshidaFlow before evalPieceShuffling
idx_shuff = find_line('evalPieceShuffling :: State -> Move -> Double')
print(f"evalPieceShuffling at line {idx_shuff + 1}")

silver_ishida_func = """-- | 銀の前線進出（棒銀・腰掛け銀・早繰り銀）および石田流本組のモース勾配流
-- 飛車の無駄なうろうろ手損を排し、力強い銀の前線進出と石田流の理想布陣を網羅
evalSilverAdvanceAndIshidaFlow :: State -> Move -> Double
evalSilverAdvanceAndIshidaFlow st m =
  let plr = turn st
      opp = opponent plr
      curBd = board st
      nextSt = applyMove m st
      nextBd = board nextSt
  in case m of
       NormalMove from to _ ->
         case Map.lookup from curBd of
           -- A. 銀の前進・前線進出（棒銀・腰掛け銀・早繰り銀）
           Just (Piece p Silver) | p == plr ->
             let (fx, fy) = from
                 (tx, ty) = to
                 isAdvancing = if plr == Sente then ty < fy else ty > fy
                 -- 1. 棒銀（Climbing Silver: 先手2筋・後手8筋を登る）
                 isClimbingSilver =
                   if plr == Sente
                   then (fx == 2 && tx == 2 && ty <= 6) || (fx `elem` [1, 2, 3] && tx `elem` [1, 2] && ty <= 5)
                   else (fx == 8 && tx == 8 && ty >= 4) || (fx `elem` [7, 8, 9] && tx `elem` [8, 9] && ty >= 5)
                 -- 2. 腰掛け銀・中央進出（Reclining / Central Silver: 5六・4六・6六）
                 isCentralSilver =
                   if plr == Sente
                   then (to == (5, 6) || to == (4, 6) || to == (6, 6) || to == (5, 5) || to == (4, 5))
                   else (to == (5, 4) || to == (6, 4) || to == (4, 4) || to == (5, 5) || to == (6, 5))
                 -- 3. 前線（敵陣近傍）進出
                 isFrontline = if plr == Sente then ty <= 5 else ty >= 5
             in if not isAdvancing then 0.0
                else if isClimbingSilver then 650.0  -- 棒銀の力強い進出！
                else if isCentralSilver  then 550.0  -- 腰掛け銀・中央厚み進出！
                else if isFrontline      then 400.0  -- 前線進出！
                else 200.0
           
           -- B. 石田流本組・三間飛車のモース勾配流
           -- 1. 飛車の動き（三間飛車 7八飛 / 3二飛、浮き飛車 7六飛 / 3四飛）
           Just (Piece p Rook) | p == plr ->
             let (fx, fy) = from
                 (tx, ty) = to
                 -- 先手石田流
                 isSenteSankanRook = plr == Sente && to == (7, 8)
                 isSenteFloatingRook = plr == Sente && from == (7, 8) && to == (7, 6) &&
                                       Map.lookup (7, 5) curBd == Just (Piece Sente Pawn)
                 -- 後手石田流
                 isGoteSankanRook = plr == Gote && to == (3, 2)
                 isGoteFloatingRook = plr == Gote && from == (3, 2) && to == (3, 4) &&
                                      Map.lookup (3, 5) curBd == Just (Piece Gote Pawn)
             in if isSenteFloatingRook || isGoteFloatingRook
                then 900.0 -- ★ 石田流浮き飛車（最強の空中戦要塞！）
                else if isSenteSankanRook || isGoteSankanRook
                then 600.0 -- 三間飛車への展開！
                else 0.0
           
           -- 2. 歩の動き（石田流の命：7五歩 / 3五歩突き）
           Just (Piece p Pawn) | p == plr ->
             let isSenteIshidaPawn = plr == Sente && from == (7, 6) && to == (7, 5)
                 isGoteIshidaPawn  = plr == Gote && from == (3, 4) && to == (3, 5)
             in if isSenteIshidaPawn || isGoteIshidaPawn
                then 700.0 -- ★ 石田流の急所歩突き！
                else 0.0
                
           -- 3. 桂馬の動き（石田流本組の桂跳ね：7七桂 / 3三桂）
           Just (Piece p Knight) | p == plr ->
             let isSenteIshidaKnight = plr == Sente && to == (7, 7) &&
                                       Map.lookup (7, 6) curBd == Just (Piece Sente Rook)
                 isGoteIshidaKnight  = plr == Gote && to == (3, 3) &&
                                       Map.lookup (3, 4) curBd == Just (Piece Gote Rook)
             in if isSenteIshidaKnight || isGoteIshidaKnight
                then 650.0 -- ★ 石田流本組の桂跳ね！
                else 0.0
                
           -- 4. 角の動き（石田流の角転回：9七角 / 1三角）
           Just (Piece p Bishop) | p == plr ->
             let isSenteCornerBishop = plr == Sente && to == (9, 7)
                 isGoteCornerBishop  = plr == Gote && to == (1, 3)
             in if isSenteCornerBishop || isGoteCornerBishop
                then 500.0
                else 0.0
           _ -> 0.0
       _ -> 0.0
"""

lines.insert(idx_shuff, silver_ishida_func + '\n')
print("1. Inserted evalSilverAdvanceAndIshidaFlow.")

# 2. Update aimlessRookPen inside evalPieceShuffling
idx_aimless = find_line('aimlessRookPen = case m of')
idx_aimless_end = find_line('in repeatPen + reversalPen + wanderingGoldPen', idx_aimless)
print(f"aimlessRookPen from line {idx_aimless + 1} to {idx_aimless_end + 1}")

new_aimless = """         -- 4. 飛車のうろうろ・無駄な手損・目的のない筋変更の徹底抑止
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
           _ -> 0.0
"""

lines[idx_aimless:idx_aimless_end] = [l + '\n' for l in new_aimless.strip().split('\n')] + ['\n         ']
print("2. Replaced aimlessRookPen with strict wandering penalties.")

# 3. Add silverAndIshidaBonus to scoreMove
idx_bishopQ = find_line('bishopDropQualityBonus = evalBishopDropQuality st m')
print(f"bishopDropQualityBonus at line {idx_bishopQ + 1}")

lines.insert(idx_bishopQ + 1, '                   silverAndIshidaBonus = evalSilverAdvanceAndIshidaFlow st m\n')
idx_sm_last = find_line('+ antiQuotientSafetyBonus + shufflingPen', idx_bishopQ)
lines[idx_sm_last] = lines[idx_sm_last].rstrip() + ' + silverAndIshidaBonus\n'
print("3. Added silverAndIshidaBonus to scoreMove.")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)

print("Saved all updates to ShogiUsiMorse-v4.hs successfully!")
