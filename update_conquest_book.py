with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

# 既存の Aigakari_StratifiedEquilibrium と GoteGokigen_TokinAssault_Equilibrium を更新
# baseBookMoves の先頭に最強必勝ラインを追加
target_header = "baseBookMoves :: [(String, [String])]\nbaseBookMoves =\n  ["

new_lines_header = '''baseBookMoves :: [(String, [String])]
baseBookMoves =
  [ ( "Kakugawari_Suisho_Conquest"
    , [ "7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "8c8d", "8h7g", "2b3c"
      , "3i3h", "7a7b", "2g2f", "7c7d", "3g3f", "4a3b", "6i7h", "7b7c"
      , "4g4f", "7c6d", "3h4g", "5a4b", "6g6f", "8d8e", "2h6h", "8a7c"
      , "2i3g", "1c1d", "4f3e", "3d3e", "4g3f", "3e3f", "3g3f", "P*3d"
      , "B*1f", "4b3c", "3f2d", "2c2d", "2h2d", "P*2c", "2d2h", "N*2e"
      , "B*4e"
      ]
    )
  , ( "Gote_Aigakari_Suisho_Conquest"
    , [ "2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f"
      , "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "5i5h", "8f8b"
      , "2d3d", "5a4b", "P*8g", "7a7b", "3d2d", "P*2c", "2d2f", "6c6d"
      , "3i3h", "6d6e", "3h2g", "8b8d", "2g2f", "8d7d", "2f2e", "7d8d"
      , "P*8f", "8d8e", "8f8e", "8a7c", "8e8d", "7c8e", "B*4e"
      ]
    )
  ,'''

content = content.replace(target_header, new_lines_header, 1)

# Aigakari_StratifiedEquilibrium の 7i7h を 6i7h（7八金型）に修正
old_aigakari = '''  [ ( "Aigakari_StratifiedEquilibrium"
    , [ "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b"
      , "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "8e8f", "8g8f", "8b8f"
      , "P*8g", "8f8b", "3i3h", "7a7b", "5i6h", "5a4b", "4i5h", "4b3c"
      , "6h7h", "7b6c", "3h2g", "8b8d", "2h2f", "8d7d", "2f2e", "7d8d"
      , "2e2f", "8d7d", "8h3c+"
      ]
    )'''

new_aigakari = '''  , ( "Aigakari_StratifiedEquilibrium"
    , [ "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "8e8f"
      , "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "5i5h", "8f8b"
      , "2d3d", "5a4b", "P*8g", "7a7b", "3d2d", "P*2c", "2d2f", "3a2b"
      , "3i3h", "7b6c", "2f2h", "8b8d", "2h2f", "8d7d", "2f2e", "7d8d"
      , "2e2f", "8d7d", "8h3c+"
      ]
    )'''

content = content.replace(old_aigakari, new_aigakari, 1)

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("SUCCESS: Updated GenerateStratifiedCompleteBook.hs with Suisho conquest lines!")
