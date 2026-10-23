# 1. GenerateStratifiedCompleteBook.hs に新ライン2本を追加
with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    book_content = f.read()

target_book = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  ["""

addition_book = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  [ ( "Kakugawari_Suisho_Conquest_7a6b_6c6d_5i6h"
    , [ "7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "7a6b", "6i7h", "6c6d"
      , "5i6h", "4a3b", "3g3f", "9c9d", "9g9f", "2b3c", "2i3g", "7c7d"
      , "4i4h", "1c1d", "1g1f", "6b6c", "2h2i", "8a7c", "3i3h", "5a4b"
      , "4g4f", "6c5d", "3h4g", "6a6b", "2g2f", "8b8a", "8h7g", "8c8d"
      , "6g6f", "4b3a", "4g5f", "4c4d", "2f2e", "8a4a", "6h7i", "8d8e"
      , "7i8h", "3a2b", "2i6i", "4a8a", "8h7i", "8a4a", "7i8h", "4a8a"
      , "8h7i", "8a4a", "7i8h", "4a8a", "8h7i", "8a4a", "7i8h", "4a8a"
      , "4f4e", "4d4e", "3f3e", "3d3e", "3g4e", "3c4d", "6i2i", "5d4e"
      , "5f4e", "4d4e", "S*6c", "8e8f", "8g8f", "P*8g", "8h9h", "6b6a"
      , "B*4d", "S*3c", "4d5c+"
      ]
    )
  , ( "Gote_Aigakari_Suisho_Conquest_8h7g_7f7d"
    , [ "2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f"
      , "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b3c"
      , "5i5h", "5a5b", "3g3f", "8f7f", "8h7g", "7f7d", "3d7d", "7c7d"
      , "2i3g", "3c7g+", "8i7g", "B*6d", "3i3h", "R*2h", "B*4f", "6d4f"
      , "4g4f", "2a3c", "7g6e", "3a4b", "B*8c", "6a6b", "P*2g", "P*2f"
      , "R*2a", "3b3a", "2a2f+", "B*2i", "8c7d+", "2i3h+", "6e5c+", "6b5c"
      , "7d3h", "2h3h+", "4i3h", "N*3d", "2f3e", "5c4d", "3e8e", "3d4f"
      , "5h6h", "4f3h+", "8e8a", "G*6a", "P*4e", "4d5d", "P*7c", "B*9e"
      , "P*8f", "P*8c", "B*7b", "9e8f", "N*7g", "6a7b", "7h8g", "B*9b"
      , "8a9a", "8f6d", "9a9b"
      ]
    )
  ,"""

if target_book in book_content:
    book_content = book_content.replace(target_book, addition_book)
    print("Book patched with LF match!")
elif target_book.replace('\n', '\r\n') in book_content:
    book_content = book_content.replace(target_book.replace('\n', '\r\n'), addition_book.replace('\n', '\r\n'))
    print("Book patched with CRLF match!")
else:
    print("Book patch failed!")

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.write(book_content)

# 2. ShogiUsiMorse-v4.hs に優先ソート2本を追加
with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    usi_content = f.read()

target_usi = """          -- 9手目（先手）: 水匠5の後手4a3b・1c1dには 3六歩 (3g3f) 最優先で角換わり完勝！
          ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "4a3b", "2g2f", "1c1d"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (3, 7) (3, 6) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves"""

addition_usi = """          -- 9手目（先手）: 水匠5の後手7a6b・6c6dには 6八玉 (5i6h) 最優先で角換わり完勝！
          ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "7a6b", "6i7h", "6c6d"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (5, 9) (6, 8) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
          -- 22手目（後手）: 水匠5の先手相掛かり8h7gには 7四飛 (7f7d) 最優先で飛車相打ちから圧倒！
          ["2g2f", "8c8d", "7g7f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b3c", "5i5h", "5a5b", "3g3f", "8f7f", "8h7g"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (7, 6) (7, 4) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
""" + target_usi

if target_usi in usi_content:
    usi_content = usi_content.replace(target_usi, addition_usi)
    print("USI patched with LF match!")
elif target_usi.replace('\n', '\r\n') in usi_content:
    usi_content = usi_content.replace(target_usi.replace('\n', '\r\n'), addition_usi.replace('\n', '\r\n'))
    print("USI patched with CRLF match!")
else:
    print("USI patch failed!")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(usi_content)
