import re

with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

# 1. isFutileIgnoredRookVanguard の定義コード
vanguard_def = [
    "\n",
    "-- | 相手の飛車先突破（8六歩/2四歩の歩突き、または8六飛/2四飛の直射）を放置する致命的手抜き手の完全除外\n",
    "-- ★ 相手の歩が突き刺さっている時は「同歩」のみを許可\n",
    "-- ★ 相手の飛車が直射していて手持ちに歩がある時は「歩打ち」のみを許可\n",
    "isFutileIgnoredRookVanguard :: State -> Move -> Bool\n",
    "isFutileIgnoredRookVanguard st m =\n",
    "  let plr = turn st\n",
    "      opp = opponent plr\n",
    "      bd = board st\n",
    "      myHand = Map.findWithDefault Map.empty plr (hands st)\n",
    "      hasPawn = Map.findWithDefault 0 Pawn myHand > 0\n",
    "      \n",
    "      -- A. 相手の歩が飛車先に突き刺さっている（先手: (8, 6)に後手歩、(8, 7)に自歩 / 後手: (2, 4)に先手歩、(2, 3)に自歩）\n",
    "      sentePawnBreach = plr == Sente && Map.lookup (8, 6) bd == Just (Piece opp Pawn) && Map.lookup (8, 7) bd == Just (Piece Sente Pawn)\n",
    "      gotePawnBreach  = plr == Gote  && Map.lookup (2, 4) bd == Just (Piece opp Pawn) && Map.lookup (2, 3) bd == Just (Piece Gote Pawn)\n",
    "      \n",
    "      -- B. 相手の飛車が直射している（先手: (8, 6)か(8, 5)に後手飛車、(8, 7)が空き / 後手: (2, 4)か(2, 5)に先手飛車、(2, 3)が空き）\n",
    "      senteRookBreach = plr == Sente && (Map.lookup (8, 6) bd == Just (Piece opp Rook) || Map.lookup (8, 5) bd == Just (Piece opp Rook)) && Map.lookup (8, 7) bd == Nothing\n",
    "      goteRookBreach  = plr == Gote  && (Map.lookup (2, 4) bd == Just (Piece opp Rook) || Map.lookup (2, 5) bd == Just (Piece opp Rook)) && Map.lookup (2, 3) bd == Nothing\n",
    "  in if sentePawnBreach\n",
    "     then case m of\n",
    "            NormalMove (8, 7) (8, 6) _ -> False -- 8六同歩のみ許可！\n",
    "            _                          -> True  -- それ以外のすべての手抜き手は完全除外！\n",
    "     else if gotePawnBreach\n",
    "     then case m of\n",
    "            NormalMove (2, 3) (2, 4) _ -> False -- 2四同歩のみ許可！\n",
    "            _                          -> True  -- それ以外のすべての手抜き手は完全除外！\n",
    "     else if senteRookBreach && hasPawn\n",
    "     then case m of\n",
    "            DropMove Pawn (8, 7) -> False -- 8七歩打のみ許可！\n",
    "            _                    -> True  -- それ以外のすべての手抜き手は完全除外！\n",
    "     else if goteRookBreach && hasPawn\n",
    "     then case m of\n",
    "            DropMove Pawn (2, 3) -> False -- 2三歩打のみ許可！\n",
    "            _                    -> True  -- それ以外のすべての手抜き手は完全除外！\n",
    "     else False\n"
]

# 挿入位置：isFutileInterpositionOnRookFile の終わりの _ -> False の後
insert_idx = None
for i in range(3000, min(len(lines), 3150)):
    if 'isFutileInterpositionOnRookFile' in lines[i]:
        # 次の _ -> False を探す
        for j in range(i, i + 30):
            if '_ -> False' in lines[j]:
                insert_idx = j + 1
                break
        break

if insert_idx is None:
    raise RuntimeError("Could not find insert position for isFutileIgnoredRookVanguard")

print(f"Inserting isFutileIgnoredRookVanguard at line {insert_idx}")
lines = lines[:insert_idx] + vanguard_def + lines[insert_idx:]

# 2. 4箇所のフィルターを更新
new_lines = []
for l in lines:
    if "safeLegalMoves = filter (\\m -> not (isPushingKakutoPawn st m)" in l:
        new_lines.append("      safeLegalMoves = filter (\\m -> not (isPushingKakutoPawn st m) && not (isFutileLanceLift st m) && not (isFutileInterpositionOnRookFile st m) && not (isFutileIgnoredRookVanguard st m)) legalMoves\n")
    elif "else filter (\\m -> not (isPushingKakutoPawn st m)" in l:
        indent = l[:l.find("else")]
        new_lines.append(f"{indent}else filter (\\m -> not (isPushingKakutoPawn st m) && not (isFutileLanceLift st m) && not (isFutileInterpositionOnRookFile st m) && not (isFutileIgnoredRookVanguard st m)) safeMoves\n")
    elif "else filter (\\m -> not (isPushingKakutoPawn state m)" in l:
        indent = l[:l.find("else")]
        new_lines.append(f"{indent}else filter (\\m -> not (isPushingKakutoPawn state m) && not (isFutileLanceLift state m) && not (isFutileInterpositionOnRookFile state m) && not (isFutileIgnoredRookVanguard state m)) safeMoves\n")
    else:
        new_lines.append(l)

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(new_lines)

print("SUCCESS: Updated ShogiUsiMorse-v4.hs successfully!")
