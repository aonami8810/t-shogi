from final_conquest_branch_lines import line_sente_branch, line_gote_branch

# 1. GenerateStratifiedCompleteBook.hs に追加
with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

target = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  ["""

sente_str = ',\n        '.join(f'"{m}"' for m in line_sente_branch)
gote_str = ',\n        '.join(f'"{m}"' for m in line_gote_branch)

addition = f"""baseBookMoves :: [(String, [String])]
baseBookMoves =
  [ ( "Kakugawari_Suisho_Conquest_7c7d_8c8d_8h7g_75p"
    , [ {sente_str}
      ]
    )
  , ( "Gote_Aigakari_Suisho_Conquest_5i5h_8f7f_75p"
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

usi_target = """          -- 9手目（先手）: 水匠5の後手2b3c・9c9dには 7八金 (6i7h) 最優先で角換わり完勝！
          ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "2b3c", "8h7g", "9c9d"] ->"""

usi_addition = """          -- 9手目（先手）: 水匠5の後手7c7d・8c8dには 7七銀 (8h7g) 最優先で角換わり完勝！
          ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "7c7d", "3g3f", "8c8d"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (8, 8) (7, 7) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
          -- 16手目（後手）: 水匠5の先手相掛かり5i5h中住まいには 7六飛 (8f7f) で横歩奪取反撃！
          ["2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "5i5h"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (8, 6) (7, 6) False -> 3 :: Int
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
