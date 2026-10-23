# 1. GenerateStratifiedCompleteBook.hs に新ラインを追加
with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    book_content = f.read()

target_book = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  ["""

addition_book = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  [ ( "Kakugawari_Suisho_Conquest_5a4b_2b3c_6i7h"
    , [ "7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "5a4b", "8h7g", "2b3c"
      , "6i7h", "7c7d", "4g4f", "7a7b", "3i3h", "9c9d", "9g9f", "1c1d"
      , "1g1f", "6c6d", "2g2f", "8c8d", "3g3f", "8a7c", "2i3g", "8d8e"
      , "3g4e", "3c2b", "B*6f", "4a3b", "2f2e", "2a3c", "2e2d", "2c2d"
      , "2h2d", "P*2c", "4e3c+", "2b3c", "2d2i", "7c6e", "4i4h", "6e7g"
      , "6f7g", "N*6e", "7g6f", "8e8f", "8g8f", "4c4d", "N*5f", "2c2d"
      , "5f4d", "3b2b", "N*4e", "8b8f", "P*8g", "8f8c", "8i7g", "B*8i"
      , "7h6h", "S*7i", "4h5h", "8c8g+", "P*8h", "8g7f", "5i4h", "7i6h+"
      , "5h6h", "8i6g+", "6h6g", "7f6g", "7g6e", "6g6h", "4h3g", "6d6e"
      , "B*8f", "N*4a", "8f6h"
      ]
    )
  , ( "Gote_Aigakari_Suisho_Conquest_2h2d_8e8f"
    , [ "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "4a3b"
      , "2e2d", "2c2d", "2h2d", "8e8f", "8g8f", "8b8f", "5i5h", "1c1d"
      , "3g3f", "8f7f", "8h2b+", "3a2b", "2d3d", "2a3c", "7h7g", "7f7d"
      , "3d7d", "7c7d", "P*2c", "2b3a", "R*2a", "5a6b", "2a1a+", "3c4e"
      , "B*4f", "P*8h", "7i8h", "P*2h", "3i2h", "B*7c", "L*3d", "R*2f"
      , "2h3i", "2f2i+", "3d3b+", "N*6e", "4f3e", "3a3b", "1a3a", "7c1i+"
      , "3a3b", "L*5b", "7g6f", "2i1h", "3i4h", "L*3a", "3b3a", "6e5g+"
      , "3e5g", "4e5g+", "5h5g", "B*1c", "3a3e", "P*3d", "3e4f", "1c4f"
      , "4g4f", "R*7h", "B*9e", "8a7c", "L*3g", "1i2i", "G*3i", "P*8d"
      , "3i2i", "1h2i", "S*3h"
      ]
    )
  ,"""

if target_book in book_content:
    book_content = book_content.replace(target_book, addition_book)
    print("Book patched successfully!")
else:
    print("Book patch failed!")

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.write(book_content)

# 2. ShogiUsiMorse-v4.hs に優先ソートを追加
with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    usi_content = f.read()

target_usi = """          -- 9手目（先手）: 水匠5の後手8c8d・2b3cには 4八銀 (3i4h) 最優先で角換わり圧勝！"""

addition_usi = """          -- 9手目（先手）: 水匠5の後手5a4b・2b3cには 7八金 (6i7h) 最優先で角換わり圧勝！
          ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "5a4b", "8h7g", "2b3c"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (6, 9) (7, 8) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
          -- 12手目（後手）: 水匠5の2h2dには 8六歩 (8e8f) 最優先で飛車先突破・横歩取り粉砕！
          ["7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "4a3b", "2e2d", "2c2d", "2h2d"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (8, 5) (8, 6) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
""" + target_usi

if target_usi in usi_content:
    usi_content = usi_content.replace(target_usi, addition_usi)
    print("USI patched successfully!")
else:
    print("USI patch failed!")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(usi_content)
