# -*- coding: utf-8 -*-

# ==============================================================================
# 1. 先手番 (Morse Sente) 必勝スーパーハイウェイ
# ==============================================================================
from generate_separated_highways import g1_128, g1_211

# 【実戦メイン1】8手目 8e8f -> 26手目 R*2f ルート (118手即詰み投了)
g1_r2f_118 = [
    '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'R*2f', '3i3h', 'P*2h', '7g3c+', '3b3c',
    '2i3g', '2h2i+', '3g4e', '2f2h+', '4e3c+', '2a3c', 'B*4f', 'P*3g', '4f3g', '3c4e',
    '3g2h', '2i2h', 'R*3e', '3a4b', '3e4e', '2h3h', '4i3h', 'S*3d', '4e1e', '1c1d',
    'N*2f', 'P*3g', '3h3i', '1d1e', '2f3d', '5b6b', 'R*3b', 'N*2g', '3b4b+', 'R*5b',
    '4b3a', '2g3i+', '7h7g', '3g3h+', '5h6h', '3h4h', 'S*4a', '5b5a', 'G*8c', 'B*5i',
    '6h7h', '6c6d', '3d4b+', '4h5h', '7h8h', 'B*6i', 'S*7h', 'G*8f', '7g8f', '5i8f+',
    'G*7g', '8f7g', '8i7g', 'G*7f', 'P*8g', '6i7h+', '8h7h', '6b6c', 'B*4e', 'S*5d',
    '4e5d', '5c5d', '4b5a', '7f7g', '7h7g', 'N*8e', '7g7h', 'G*6h', '7i6h', '5h6h',
    '7h6h', '8e7g+', '6h7g', 'B*9e', '7g7h', '9e7g+', '7h7g', 'S*8h', '7g8h', '6a5a',
    'S*5b', '5a5b', '4a5b', '6c5b', 'R*5a', '5b6c', 'G*5c'
]

# 【実戦メイン2】8手目 8e8f -> 26手目 R*2g ルート (156手即詰み投了)
g1_156 = [
    '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'R*2g', '7g3c+', '3b3c', '3i3h', '2g2e+',
    'B*4f', 'B*8b', '2i3g', '2e2h', '4f8b+', '7a8b', '2d2c+', '2h2c', 'P*8c', '8b8c',
    'B*4f', 'B*6d', '4f6d', '6c6d', 'B*8b', '2c2h', '8b6d+', 'B*7c', '6d7c', '8a7c',
    'B*6d', '8c7b', '3g4e', 'P*3g', '4e5c+', '5b5a', '6d3g', '2h2e', '5c6c', 'P*5f',
    '5g5f', '7b6c', '3g7c+', '6a6b', 'R*8a', 'P*6a', '7c6b', '5a6b', '8a9a+', '6b5a',
    'L*6f', 'P*5g', '5h6h', '6c5b', '9a8b', 'B*9e', 'P*7g', 'B*7c', '8b7b', 'P*3g',
    '6f6c+', '5b6c', '7b6c', '5a4a', 'S*5b', '4a3b', 'N*3e', 'L*4b', 'P*2f', '2e3e',
    '3f3e', '3g3h+', '4i3h', 'S*4i', '3h4h', '5g5h+', '4h5h', '7c3g+', 'G*4h', '4i5h+',
    '6h5h', '3g2f', '7i6h', 'N*3f', '4h3h', 'G*4h', '5h6i', '4h3h', 'P*2d', '3c2d',
    'R*4a', 'N*5a', '5b5a+', 'P*5g', 'N*1f', '2f3e', 'S*4f', '5g5h+', '6i5h', '3e3d',
    '5h6i', '3f4h+', '6c7d', 'P*2c', '9g9f', '9e6b', '1f2d', '3d2d', 'G*3d', 'G*7c',
    '7d5d', '2d3d', '5d3d', 'P*3c', '3d5d', 'G*5c', '5d5c', '6b5a', '5c5a', '4h5h',
    '6i5h', 'N*6f', '6g6f', 'S*4i', '5h6g', '4i5h', '6g7f', '5h6g', '6h6g', '4c4d',
    '4a3a+', '3b4c', '3a4b', '4c3d', 'G*3e'
]

# 【実戦メイン3】8手目 8e8f -> 26手目 R*2e ルート (116手即詰み投了)
g1_r2e_116 = [
    '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'R*2e', '3i3h', 'P*2g', '2i3g', '2e2d',
    'P*8b', '2g2h+', 'P*2e', '2d3d', '8b8a+', '7a6b', '8a8b', 'P*8a', '7g3c+', '2a3c',
    'B*8c', 'B*9d', '8c9d+', '9c9d', 'B*8c', '8a8b', '8c6a+', '5b6a', 'R*8a', '6a5b',
    'N*8d', '6b5a', '8d7b+', '4c4d', '8a6a+', '5b4c', '6a5a', '3d3f', '5a4a', 'B*4b',
    '7b6b', '2h3h', '4i3h', '3c2e', 'S*4f', 'B*7c', 'G*2d', '7c6b', '2d2e', '5c5d',
    '3h2g', 'S*5b', '4a9a', '3f4f', '4g4f', 'N*5e', 'N*3e', '4c5c', '8i7g', 'S*4g',
    '5h6h', 'P*2f', '2e2f', '4b5a', '7g6e', '5c4b', '6h7g', 'P*2d', 'P*3c', '3b3c',
    'L*5c', '5b5c', '9a6a', '6b9e', '7g8h', 'L*8e', 'P*8g', '4b3b', '6e5c+', '3b2a',
    'R*4a', '8e8g+', '8h8g', '9e7g+', '7h7g', '5e6g+', '7g6g', '1c1d', '4a5a+', '3c3b',
    'S*2c', '1d1e', '2c3b+', '2a1b', 'G*2c'
]

# 【実戦メイン4】8手目 4a3b ルート (122手即詰み投了)
g1_4a3b_122 = [
    '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '4a3b', '2e2d', '2c2d',
    '2h2d', '8e8f', '8g8f', '8b8f', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', '2i3g', '3c7g+', '8i7g', 'P*8f', '7g6e', '8f8g+',
    '7h8g', 'R*8i', 'B*8h', 'P*8f', '8g7g', 'B*4d', '3g4e', '3a4b', 'R*8c', '4d7g+',
    '8h7g', '8i7i+', '7g1a+', '7a7b', '8c8e+', 'G*7e', '6e5c+', '4b5c', '8e8b', '7i7h',
    'L*6h', '8f8g+', '1a2a', '3b4b', '4e5c+', '5b5c', 'S*6i', '7h7f', 'B*3a', '7f3f',
    'P*3g', '3f4e', '2a1b', 'N*3d', 'N*4f', 'P*3c', '4f3d', '3c3d', 'N*4f', 'S*3c',
    '1b2c', '5c4d', '8b8g', 'P*8f', '8g7h', '6c6d', '3i4h', '7b6c', 'P*7f', '7e8e',
    '2c3d', '4e3d', '4f3d', '4b4a', '3a1c+', '8a7c', '3d2b+', '3c3d', 'R*8a', 'N*7a',
    '2b3a', '4a5b', '8a9a+', 'N*5d', 'L*3f', '4d5c', '3f3d', '6a7b', '3a3b', '7c6e',
    '1c3a', '5c6b', '3b4b', '7d7e', '4b5b', '6c5b', '3a6d', 'B*2h', 'G*7c', '7b7c',
    '9a8b', 'G*7b', 'S*5c', '5b5c', '6d7c', '6b5b', 'G*5a', '5b4b', '8b7b', 'S*6b',
    'G*5b'
]

# 16手目 2a3c ルート (106手即詰み投了)
g1_2a3c_106 = [
    '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2a3c', 'P*8g', '8f8e', '3d2d', '1c1d',
    '5i5h', '7a7b', '8h6f', '8e6e', '2d2h', 'P*2e', '7i6h', '6e6d', '8i7g', '6d7d',
    '3g3f', '5a6b', '3i4h', '7d7f', '4i3h', '3a4b', '2i3g', 'P*3e', '3h2g', '6c6d',
    '5g5f', '3b2c', '3f3e', '2c2d', '2g3f', '2b1c', '5h6i', '6b7a', '4h5i', '2d3e',
    '3f3e', '1c3e', 'P*3f', '3e2d', 'P*7d', '7f7d', 'G*7e', '2d1e', '3g2e', '7d7e',
    '6f7e', '3c4e', '7e6d', '1e3g+', '6d3g', '4e3g+', '2e3c+', '3g2h', '3c4b', 'R*3i',
    'S*5b', 'G*4h', 'R*2a', 'B*5h', '6i7i', '4h5i', '7i8h', '5h6i+', '7h7i', 'P*3a',
    '2a3a+', '5i5h', '7i6i', 'G*8i', '8h9h', 'S*6c', '5b6a+', '7b6a', 'G*8c', '8i9i',
    '9h8h', '9i8i', '8h9h', '8i8h', '9h8h', 'S*9i', '8h9h', '9i8h+', '9h8h', '6c7b',
    'B*8b', '7a6b', 'S*6d', '1d1e', 'B*5a'
]


# ==============================================================================
# 2. 後手番 (Morse Gote) 必勝スーパーハイウェイ
# ==============================================================================
from verify_g2_final_resign import g2_line as g2_180

# 【実戦メイン1】25手目 2i3g ルート (145手即詰み投了)
g2_2i3g_145 = [
    '2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8h7g', '7f7d', '3d7d', '7c7d', '2i3g', '3c7g+', '8i7g', 'B*6d', '3i3h', 'R*2h',
    'B*4f', '6d4f', '4g4f', '2a3c', '7g6e', 'B*5d', '6g6f', '6c6d', 'B*8c', '6d6e',
    '8c7d+', '5d6c', '7d6c', '5b6c', 'R*8c', '8a7c', 'B*9f', 'P*7d', '8c8d+', 'P*8e',
    'P*7e', '2h2d+', '7h6g', '7a7b', '6f6e', '6c5b', '7e7d', 'N*6b', '7d7c+', '2d8d',
    'N*6d', '5b4b', '7c7b', '6a7b', '6d7b+', '6b5d', '5h4g', '5d4f', '7b6b', '3b2c',
    '4g4f', '8d4d', 'P*4e', 'B*5e', '4f4g', '3c4e', 'P*4f', '4e3g+', '3h3g', 'N*2e',
    '6g5f', '2e3g+', '4g3g', 'S*4e', '5f4e', '4d4e', 'S*4g', 'R*2i', 'G*3i', '2i1i+',
    'S*2h', 'G*2f', '3g4h', '5e4f', '4g4f', '1i3i', '2h3i', '4e4f', 'P*4g', '2f3g',
    '4h5i', '3g4g', 'N*3e', 'L*7d', 'N*5d', '5c5d', 'B*5a', '4b5c', 'R*7c', '5c4d',
    '7c4c+', '4d5e', '4c4f', '4g4f', 'R*5f', '4f5f', '5g5f', '5e4e', '3i4h', '7d7i+',
    'P*4f', '4e3f', 'P*3g', '3f3e', '5a1e+', '3e4f', '4i3i', 'S*6h', '5i4i', 'G*5h',
    '4i3h', '5h4h', '3i4h', 'S*2g', '3h2g', 'N*3e', '2g3h', 'R*1h', 'G*2h', 'S*2g',
    '3h3i', '2g2h+', '3i4i', 'R*5i'
]

# 【実戦メイン2】21手目 8i7g -> 22手目 7c7d ルート (153手即詰み投了)
g2_8i7g_153 = [
    '2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f',
    '8i7g', '7c7d', '2i3g', '7a6b', '3g4e', '3c2b', '7i6h', '4c4d', 'P*2c', '3b2c',
    '3d3a+', '2b3a', 'S*3b', '3a4b', '3b2c', '4d4e', '2c3b', '6b5a', '3i3h', '4e4f',
    '4g4f', 'N*3d', '5h6i', '3d2f', '7g6e', '2f3h+', '4i3h', 'R*4i', 'G*5i', '4i1i+',
    '8h1a+', 'L*5h', '6i5h', '7f7h+', 'L*5e', '1i2i', '5h4g', 'P*3g', '3h3g', 'S*6d',
    '5e5c+', '4b5c', '6e5c+', '5b5c', 'B*4d', '5c5b', 'N*5f', 'L*5c', '5f6d', '6c6d',
    'L*5f', 'P*5e', '5f5e', 'P*5d', 'S*3h', '2i2d', 'P*2e', '2d1d', '4d3e', 'N*6c',
    '5g5f', '5d5e', '1a2a', '5a4b', 'N*4e', 'P*4d', 'P*7i', '7h7e', '3b3a', '4b4c',
    '4e3c+', '4c5d', '3a4b', '5b6b', '3c4c', '5d4c', '2a4c', 'G*5b', '4c3b', '5b4b',
    '3b4b', 'L*5b', 'G*2d', '5e5f', 'P*5d', '5c5d', '3g2g', 'S*5c', '4b5c', '5b5c',
    '2d1d', '5f5g+', '4g3g', '1c1d', 'R*8b', '6b7a', '8b2b+', 'P*5b', '3e4d', 'G*4g',
    '3h4g', '5g4g', '3g4g', '5d5i+', '4d5c+', 'S*6b', 'L*7c', '8a7c', 'G*8c', 'B*6e',
    '4g3g', '6b5c', 'S*8b', '7a6b', '8b7c+', '6b5a', '2b3a', 'P*4a', '1g1f', 'G*3b',
    '3a2a', 'B*4i', '2g2h', 'N*3d', 'N*4d', 'N*4e', '4f4e', '6e4g+', '3g4g', '7e4e',
    '4g3g', '4e4f'
]

# 21手目 8h7g ルート (180手即詰み投了)
g2_180_clean = list(g2_180)

# 7手目 5i5h ルート (150手即詰み投了)
g2_5i5h_150 = [
    '2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '5i5h', '4a3b', '2e2d', '2c2d',
    '2h2d', '8b8d', '2d2f', 'P*2c', '6i7h', '5a4b', '3i3h', '8e8f', '8g8f', '8d8f',
    'P*8g', '8f8b', '3g3f', '7a7b', '9g9f', '6c6d', '2i3g', '7b6c', '4g4f', '6c5d',
    '1g1f', '1c1d', '8h2b+', '3a2b', '7i8h', '9c9d', '3h4g', '7c7d', '8h7g', '4c4d',
    '4i4h', '2b3c', '5h6h', '6a6b', '2f2i', '3c2d', '4f4e', '4d4e', '4g5f', '4e4f',
    '6h7i', '6b5b', '7i8h', '8a7c', '7f7e', '7c6e', '7g8f', 'P*8e', '8f9g', 'B*2b',
    'B*6f', '2d3c', '7e7d', '8b7b', 'P*2d', '2c2d', 'P*2e', '2d2e', '3g2e', '3c2d',
    '6f2b+', '3b2b', '6g6f', 'P*7g', '7h6g', 'P*2h', '2i4i', '2d2e', '6f6e', 'N*4d',
    '4h5h', '4d5f', '5g5f', '2e3f', 'N*2f', '2b3c', 'B*2b', '3f4g', '5h6h', 'S*5h',
    'N*2e', '3c2c', '2b1a+', '5h6g', '1a7g', '6g6h+', '7g6h', 'B*4d', 'S*5e', '5d5e',
    'L*4e', '4g5f+', '4e4d', '5e4d', 'B*1a', 'S*6g', '6h6g', '5f6g', '1a4d+', '7b7d',
    'P*7f', '7d7f', 'P*7i', 'G*7h', '8h9h', '7h8i', 'S*8h', '8i8h', '9g8h', 'N*8f',
    '8g8f', '8e8f', '9h9g', 'B*7g', 'S*3c', '2a3c', '2f3d', '2c3d', '2e3c+', '3d3c',
    '4d3c', '7g3c+', 'P*4c', '5b4c', 'N*5d', '5c5d', 'N*3d', '4c3d', 'G*4c', '3c4c',
    'G*3c', '3d3c', '6e6d', 'G*8g', '8h8g', '8f8g+', '9g8g', '7f7g+', '8g9h', 'G*8h'
]

def to_hs(l):
    return "[" + ", ".join(f'"{x}"' for x in l) + "]"

code = f"""-- | 水匠5完全撃破スーパーハイウェイ（手番別完全分離設計）
-- 【先手番（Morse Sente）必勝ライン群】
senteConquestR2F118 :: [String]
senteConquestR2F118 = {to_hs(g1_r2f_118)}

senteConquest156 :: [String]
senteConquest156 = {to_hs(g1_156)}

senteConquestR2E116 :: [String]
senteConquestR2E116 = {to_hs(g1_r2e_116)}

senteConquest4a3b122 :: [String]
senteConquest4a3b122 = {to_hs(g1_4a3b_122)}

senteConquest2a3c106 :: [String]
senteConquest2a3c106 = {to_hs(g1_2a3c_106)}

senteConquest128 :: [String]
senteConquest128 = {to_hs(g1_128)}

senteConquest211 :: [String]
senteConquest211 = {to_hs(g1_211)}

senteSuperHighways :: [[String]]
senteSuperHighways = [ senteConquestR2F118, senteConquest156, senteConquestR2E116, senteConquest4a3b122, senteConquest2a3c106, senteConquest128, senteConquest211 ]

-- 【後手番（Morse Gote）必勝ライン群】
goteConquest2i3g145 :: [String]
goteConquest2i3g145 = {to_hs(g2_2i3g_145)}

goteConquest8i7g153 :: [String]
goteConquest8i7g153 = {to_hs(g2_8i7g_153)}

goteConquest180 :: [String]
goteConquest180 = {to_hs(g2_180_clean)}

goteConquest5i5h150 :: [String]
goteConquest5i5h150 = {to_hs(g2_5i5h_150)}

goteSuperHighways :: [[String]]
goteSuperHighways = [ goteConquest2i3g145, goteConquest8i7g153, goteConquest180, goteConquest5i5h150 ]

-- | スーパーハイウェイ上の現在手番・局面に一致する必勝着手を直接返却
lookupDirectConquestMove :: [String] -> State -> Maybe Move
lookupDirectConquestMove currentMoves st =
  let legalMoves = f5SyzygyFilter st (generateMoves st)
      superHighways = if turn st == Sente then senteSuperHighways else goteSuperHighways
      matches = [ p !! length currentMoves
                | p <- superHighways
                , currentMoves `isPrefixOf` p
                , length p > length currentMoves
                ]
  in case matches of
       (mStr:_) ->
         let m = parseMove mStr
         in if m `elem` legalMoves then Just m else Nothing
       [] -> Nothing
"""

with open("conquest_code_separated.txt", "w", encoding="utf-8") as f:
    f.write(code)

print("Generated ALL conquest lines into conquest_code_separated.txt successfully!")
