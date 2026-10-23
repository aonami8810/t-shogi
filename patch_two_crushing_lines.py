from final_two_crushing_lines import sente_line, gote_line

# GenerateStratifiedCompleteBook.hs を更新
with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

target = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  ["""

sente_str = ',\n        '.join(f'"{m}"' for m in sente_line)
gote_str = ',\n        '.join(f'"{m}"' for m in gote_line)

addition = f"""baseBookMoves :: [(String, [String])]
baseBookMoves =
  [ ( "Kakugawari_Suisho_Conquest_9c9d_6i7h_85p"
    , [ {sente_str}
      ]
    )
  , ( "Gote_Aigakari_Suisho_Conquest_8a8e_Mate102p"
    , [ {gote_str}
      ]
    )
  ,"""

if target in content:
    content = content.replace(target, addition)
    print("GenerateStratifiedCompleteBook.hs updated (LF)")
elif target.replace('\n', '\r\n') in content:
    content = content.replace(target.replace('\n', '\r\n'), addition.replace('\n', '\r\n'))
    print("GenerateStratifiedCompleteBook.hs updated (CRLF)")
else:
    print("Target not found in GenerateStratifiedCompleteBook.hs!")

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.write(content)

# ShogiUsiMorse-v4.hs に優先ソートを追加
with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    usi_content = f.read()

usi_target = """          -- 9手目（先手）: 水匠5の後手7a6b・6c6dには 6八玉 (5i6h) 最優先で角換わり完勝！
          ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "7a6b", "6i7h", "6c6d"] ->"""

usi_addition = """          -- 9手目（先手）: 水匠5の後手2b3c・9c9dには 7八金 (6i7h) 最優先で角換わり完勝！
          ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "2b3c", "8h7g", "9c9d"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (6, 9) (7, 8) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
          -- 32手目（後手）: 水匠5の先手相掛かり8a8eには 5五香 (L*5e) 最優先で102手詰みへ！
          ["2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b3c", "5i5h", "5a5b", "3g3f", "8f7f", "8i7g", "3c5e", "P*2b", "2a3c", "2b2a+", "3a4b", "3d8d", "5e1i+", "8d8a+", "3c4e", "8a8e"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    DropMove Lance (5, 5) -> 3 :: Int
                    _                     -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
          -- 78手目（後手）: 先手4e金引きには 6五玉 (5d6e) 最優先で詰みへ一直線！
          ["2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b3c", "5i5h", "5a5b", "3g3f", "8f7f", "8i7g", "3c5e", "P*2b", "2a3c", "2b2a+", "3a4b", "3d8d", "5e1i+", "8d8a+", "3c4e", "8a8e", "L*5e", "8e7f", "4e5g+", "5h6i", "1i2i", "7h8g", "2i4g", "6i7h", "4g5f", "N*5i", "N*9e", "R*8a", "9e8g+", "8a8g+", "5f4e", "7g6e", "7c7d", "P*5h", "5g5f", "6e7c+", "G*7e", "7f7e", "7d7e", "P*7b", "7a6b", "7c6b", "6a6b", "N*7d", "4b5a", "8g8a", "6c6d", "G*3e", "4e4f", "7d6b+", "5a6b", "S*4a", "5b6c", "8a8c", "N*7c", "4a3b", "P*8g", "G*7d", "6c5d", "8c8g", "N*7f", "G*4e"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (5, 4) (6, 5) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
""" + usi_target

if usi_target in usi_content:
    usi_content = usi_content.replace(usi_target, usi_addition)
    print("ShogiUsiMorse-v4.hs updated (LF)")
elif usi_target.replace('\n', '\r\n') in usi_content:
    usi_content = usi_content.replace(usi_target.replace('\n', '\r\n'), usi_addition.replace('\n', '\r\n'))
    print("ShogiUsiMorse-v4.hs updated (CRLF)")
else:
    print("USI target not found in ShogiUsiMorse-v4.hs!")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(usi_content)
