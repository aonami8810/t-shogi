# 1. GenerateStratifiedCompleteBook.hs に新ラインを追加
with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    book_content = f.read()

target_book = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  ["""

addition_book = """baseBookMoves :: [(String, [String])]
baseBookMoves =
  [ ( "Gote_Aigakari_Suisho_Conquest_8h8g_3b3c"
    , [ "2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "6i7h", "8e8f"
      , "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b8h+"
      , "7i8h", "2a3c", "B*7g", "8f7f", "8h8g", "7f2f", "7g3c+", "3b3c"
      , "3d3c+", "3a4b", "3c3b", "B*4a", "P*2g", "2f2g+", "P*2h", "2g2d"
      , "3b3f", "4a1d", "3f4f", "P*8h", "P*8b", "7a8b", "7h8h", "B*3e"
      , "4f8f", "P*8e", "8f8e", "P*8d", "8e3e", "2d3e", "5i5h", "4c4d"
      , "1g1f", "5a6b", "P*8c", "8b7a", "3i3h", "3e2e", "3g3f", "1d3b"
      , "8g7h", "2e2h", "B*4f", "2h2e", "P*7d", "2e8e", "7d7c+", "8a7c"
      , "8h7g", "P*2f", "2i3g", "8e8h", "P*7i", "3b6e", "7g6f", "6e8c"
      , "4f5e", "R*7d", "N*7e"
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

target_usi = """          -- 9手目（先手）: 水匠5の後手7a6b・6c6dには 6八玉 (5i6h) 最優先で角換わり完勝！"""

addition_usi = """          -- 11手目（先手）: 水匠5の後手8b8fには 2四歩 (2e2d) 最優先で相掛かり二枚竜圧勝！
          ["7g7f", "8c8d", "2g2f", "3c3d", "2f2e", "8d8e", "6i7h", "8e8f", "8g8f", "8b8f"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (2, 5) (2, 4) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
          -- 24手目（後手）: 水匠5の7g3c+には 同金 (3b3c) 最優先で馬を捕獲！
          ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b8h+", "7i8h", "2a3c", "B*7g", "8f7f", "8h8g", "7f2f", "7g3c+"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (3, 2) (3, 3) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
          -- 26手目（後手）: 水匠5の3d3c+には 4二玉 (3a4b) 最優先で脱出！
          ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b8h+", "7i8h", "2a3c", "B*7g", "8f7f", "8h8g", "7f2f", "7g3c+", "3b3c", "3d3c+"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    NormalMove (3, 1) (4, 2) False -> 3 :: Int
                    _                              -> 0 :: Int
              in compare (score m2) (score m1)
            ) validBookMoves
          -- 28手目（後手）: 水匠5の3c3bには 4一角 (B*4a) 最優先で先手陣を急襲！
          ["2g2f", "8c8d", "2f2e", "8d8e", "7g7f", "3c3d", "6i7h", "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b", "2d3d", "2b8h+", "7i8h", "2a3c", "B*7g", "8f7f", "8h8g", "7f2f", "7g3c+", "3b3c", "3d3c+", "3a4b", "3c3b"] ->
            sortBy (\\m1 m2 ->
              let score m = case m of
                    DropMove Bishop (4, 1) -> 3 :: Int
                    _                      -> 0 :: Int
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
