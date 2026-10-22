with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

def find_line(pat, start=0):
    for i in range(start, len(lines)):
        if pat in lines[i]:
            return i
    return -1

# 1. Insert isTrappedPiece and evalTrappedPieceDesperado before evalPieceShuffling
idx_shuff = find_line('evalPieceShuffling :: State -> Move -> Double')
print(f"evalPieceShuffling at line {idx_shuff + 1}")

desperado_code = """-- | 攻め駒（飛・角・銀・桂・竜・馬）が相手の駒に包囲・詰まされている（Trapped）かどうかの判定
isTrappedPiece :: Square -> PieceType -> Player -> Board -> Bool
isTrappedPiece sq pt plr bd =
  let opp = opponent plr
      oppAttackers = getPiecesCovering opp sq bd
  in if null oppAttackers
     then False -- 当たりになっていなければ詰まされていない
     else
       let candidateDests = [ dest | dest <- Map.keys bd ++ [ (x, y) | x <- [1..9], y <- [1..9] ]
                                   , canPieceReachTarget sq (Piece plr pt) dest bd
                                   , case Map.lookup dest bd of
                                       Just (Piece p _) -> p /= plr
                                       Nothing          -> True
                            ]
           isSafeSquare dest =
             let oppAtk = getPiecesCovering opp dest bd
                 myDef  = getPiecesCovering plr dest bd
                 hasCheap = any (\\(_, Piece _ opt) -> opt `elem` [Pawn, Lance, Knight, Silver]) oppAtk
             in null oppAtk || (length myDef > length oppAtk && not hasCheap)
           
           hasSafeRetreat = any isSafeSquare candidateDests
       in not hasSafeRetreat -- 安全な退避先が1つもなければ「詰まされている（Trapped）」！

-- | 攻め駒が詰まされた場合のモース勾配流（Desperado 流）
-- 価値の低い駒交換（歩や香との刺し違え・タダ死に）を厳罰排除し、
-- 最大価値の駒（金・銀・角・飛）との刺し違え、または急所破壊・王手を最優先化
evalTrappedPieceDesperado :: State -> Move -> Double
evalTrappedPieceDesperado st m =
  let plr = turn st
      opp = opponent plr
      bd = board st
      nextSt = applyMove m st
      nextBd = board nextSt
      isCheck = isKingUnderAttack opp nextBd
  in case m of
       NormalMove from to _ ->
         case Map.lookup from bd of
           Just (Piece p pt) | p == plr && pt `elem` [Rook, PRook, Bishop, PBishop, Silver, PSilver, Knight] ->
             let isTrapped = isTrappedPiece from pt plr bd
             in if not isTrapped
                then 0.0
                else
                  let targetCap = Map.lookup to bd
                  in case targetCap of
                       -- 1. 相手の駒を取る手（刺し違え）
                       Just (Piece _ capPt) ->
                         let capVal = pieceValue capPt
                         in if capVal >= 500.0
                            then 3200.0 + capVal * 4.0 -- 金・銀・角・飛との最大刺し違え（大成功の Desperado！）
                            else if capVal >= 300.0
                            then (if pt `elem` [Rook, Bishop] then -2500.0 else 600.0) -- 桂・香との刺し違え
                            else
                              -- ★ 歩との刺し違え（最悪の低価値駒交換！角歩交換・銀歩交換・飛車歩交換）
                              if pt `elem` [Rook, PRook, Bishop, PBishop]
                              then -9000.0 -- 大駒で歩を取って死ぬのは絶対禁止！
                              else if pt `elem` [Silver, PSilver]
                              then -6000.0 -- 銀で歩を取って死ぬのも絶対禁止！
                              else -3500.0 -- 桂で歩を取って死ぬのも禁止！
                       -- 2. 駒を取らずに逃げる手
                       Nothing ->
                         let oppAtkOnTo = getPiecesCovering opp to nextBd
                             myDefOnTo  = getPiecesCovering plr to nextBd
                             isStillDead = not (null oppAtkOnTo) && (null myDefOnTo || any (\\(_, Piece _ opt) -> opt == Pawn) oppAtkOnTo)
                         in if isCheck
                            then 800.0 -- 王手で陣形を崩す手筋
                            else if isStillDead
                            then -7500.0 -- 駒も取れずに結局歩などにタダ取られする無駄な逃げ
                            else 0.0
           _ -> 0.0
       DropMove _ _ -> 0.0
"""

lines.insert(idx_shuff, desperado_code + '\n')
print("1. Inserted isTrappedPiece and evalTrappedPieceDesperado.")

# 2. Update scoreMove to include trappedDesperadoBonus
idx_sm = find_line('in s + captureBonus + blunderPen')
print(f"scoreMove return at line {idx_sm + 1}")

lines[idx_sm] = '                   trappedDesperadoBonus = evalTrappedPieceDesperado st m\n                   s = evaluateState weights (turn st) nextSt\n'
lines[idx_sm + 1] = lines[idx_sm + 1].rstrip() + ' + trappedDesperadoBonus\n'
print("2. Integrated trappedDesperadoBonus into scoreMove.")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)

print("Saved all changes to ShogiUsiMorse-v4.hs successfully!")
