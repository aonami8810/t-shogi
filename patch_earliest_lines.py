from earliest_two_lines import line_sente_early, line_gote_early

# 1. GenerateStratifiedCompleteBook.hs に追加
with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

target = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  ["""

sente_str = ',\n        '.join(f'"{m}"' for m in line_sente_early)
gote_str = ',\n        '.join(f'"{m}"' for m in line_gote_early)

addition = f"""baseBookMoves :: [(String, [String])]
baseBookMoves =
  [ ( "Kakugawari_Suisho_Conquest_8d8e_8h7g_75p"
    , [ {sente_str}
      ]
    )
  , ( "Gote_Aigakari_Suisho_Conquest_6i7h_8d8e_75p"
    , [ {gote_str}
      ]
    )
  ,"""

if target in content:
    content = content.replace(target, addition)
    print("GenerateStratifiedCompleteBook.hs patched (LF)")
elif target.replace('\n', '\r\n') in content:
    content = content.replace(target.replace('\n', '\r\n'), addition.replace('\n', '\r\n'))
    print("GenerateStratifiedCompleteBook.hs patched (CRLF)")
else:
    print("GenerateStratifiedCompleteBook.hs target not found!")

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.write(content)

# 2. ShogiUsiMorse-v4.hs に優先ソートを追加
with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    usi_content = f.read()

usi_target = """          -- 9手目（先手）: 水匠5の後手8c8d・7a6bには 4八銀 (3i4h) 最優先で角換わり完勝！
          ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "8c8d", "2g2f", "7a6b"] ->"""

usi_addition = """          -- 5手目（先手）: 水匠5の後手2手目8c8d・4手目8d8eには 7七銀 (8h7g) 最優先で角換わり完勝！
          ["7g7f", "8c8d", "2g2f", "8d8e"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (8, 8) (7, 7) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
          -- 6手目（後手）: 水匠5の先手相掛かり5手目6i7hには 8五歩 (8d8e) 最優先で飛車先突破完勝！
          ["2g2f", "8c8d", "7g7f", "3c3d", "6i7h"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (8, 4) (8, 5) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
""" + usi_target

if usi_target in usi_content:
    usi_content = usi_content.replace(usi_target, usi_addition)
    print("ShogiUsiMorse-v4.hs patched (LF)")
elif usi_target.replace('\n', '\r\n') in usi_content:
    usi_content = usi_content.replace(usi_target.replace('\n', '\r\n'), usi_addition.replace('\n', '\r\n'))
    print("ShogiUsiMorse-v4.hs patched (CRLF)")
else:
    print("ShogiUsiMorse-v4.hs target not found!")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(usi_content)
