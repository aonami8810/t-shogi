# 1. GenerateStratifiedCompleteBook.hs に新ラインを追加
with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    book_content = f.read()

target_book = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  ["""

addition_book = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  [ ( "Kakugawari_Suisho_Conquest_8c8d_2b3c_3i4h"
    , [ "7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "8c8d", "2g2f", "2b3c"
      , "3i4h", "5a4b", "4g4f", "7c7d", "6i7h", "7a7b", "4h4g", "7b7c"
      , "3g3f", "6a5b", "4g5f", "8d8e", "2f2e", "4c4d", "5i6h", "7c6d"
      , "8h7g", "4b3b", "7g6f", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b"
      , "4i4h", "4a4b", "2i3g", "5b4c", "1g1f", "1c1d", "9g9f", "B*5d"
      , "4f4e", "5d7f", "2e2d", "3c2d", "6f7g", "7f5d", "4e4d", "4c4d"
      , "B*6f", "4b3c", "P*4e", "4d4c", "3g2e", "2d2e", "2h2e", "6d7e"
      , "6f5e", "7e6d", "5e6f", "P*4b", "S*5e", "6d5e", "6f5e", "S*6d"
      , "5e6f", "8a7c", "2e2i", "6d7e", "6f5e", "N*6d", "5f4g", "5d6e"
      , "6h5h", "5c5d", "5e4f"
      ]
    )
  , ( "Gote_Aigakari_Suisho_Conquest_9g9f_1c1d"
    , [ "2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "4a3b", "9g9f", "1c1d"
      , "3i3h", "7a7b", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "5a4b"
      , "4g4f", "9c9d", "3h4g", "3c3d", "1g1f", "7c7d", "7g7f", "8e8f"
      , "8g8f", "8b8f", "4g5f", "8f8b", "P*8g", "6c6d", "5f4e", "3b3c"
      , "8h3c+", "4b3c", "1f1e", "8a7c", "1e1d", "3c4b", "7i8h", "7c6e"
      , "8i7g", "6e7g+", "8h7g", "N*6e", "7g6f", "2b6f", "6g6f", "S*7g"
      , "7h7g", "6e7g+", "N*8f", "G*4g", "4i5h", "4g5h", "5i5h", "2a3c"
      , "4e5f", "7g8g", "8f7d", "8b8e", "P*8f", "8e8f", "2h2c+", "G*2b"
      , "2c3d", "8g7g", "5h4g", "B*6i", "S*5h", "6i5h+", "4g5h", "8f8i+"
      , "G*5i", "8i8h", "5h4g"
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

target_usi = """          -- 11手目（先手）: 水匠5の後手8b8fには 2四歩 (2e2d) 最優先で相掛かり二枚竜圧勝！"""

addition_usi = """          -- 9手目（先手）: 水匠5の後手8c8d・2b3cには 4八銀 (3i4h) 最優先で角換わり圧勝！
          ["7g7f", "3c3d", "8h2b+", "3a2b", "7i8h", "8c8d", "2g2f", "2b3c"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (3, 9) (4, 8) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
          -- 8手目（後手）: 水匠5の先手9g9fには 1四歩 (1c1d) 最優先で端歩受け完封！
          ["2g2f", "8c8d", "2f2e", "8d8e", "6i7h", "4a3b", "9g9f"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (1, 3) (1, 4) False -> 3 :: Int
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
