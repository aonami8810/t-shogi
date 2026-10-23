with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

target = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  ["""

addition = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  [ ( "Kakugawari_Suisho_Conquest_4a3b_1c1d_3g3f"
    , [ "7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "4a3b", "2g2f", "1c1d"
      , "3g3f", "7a7b", "3i3h", "8c8d", "3h3g", "6c6d", "5i6h", "8d8e"
      , "8h7g", "7b6c", "2f2e", "6c5d", "6h7h", "7c7d", "6i6h", "8a7c"
      , "2e2d", "2c2d", "2h2d", "6a6b", "2d2h", "5a4b", "5g5f", "P*2d"
      , "2h2d", "8e8f", "8g8f", "7c6e", "2d2h", "6e7g+", "6h7g", "8b8a"
      , "3f3e", "5d6e", "N*4f", "4b5b", "B*3f", "8a7a", "4f3d", "2b3c"
      , "P*2d", "7d7e", "7f7e", "3c2d", "2h2d", "B*3c", "S*8b", "7a6a"
      , "2d2h", "P*7f", "7g6f", "6e6f", "6g6f", "3c6f", "P*6c", "6b7b"
      , "S*6b", "6a3a", "6b7c+", "7b7c", "8b7c+", "G*7g", "7h6i", "S*7h"
      , "2h7h", "7g7h", "6i7h"
      ]
    )
  ,"""

if target in content:
    content = content.replace(target, addition)
    print("Replaced with LF match!")
elif target.replace('\n', '\r\n') in content:
    content = content.replace(target.replace('\n', '\r\n'), addition.replace('\n', '\r\n'))
    print("Replaced with CRLF match!")
else:
    print("Match failed!")

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.write(content)
