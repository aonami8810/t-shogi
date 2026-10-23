sente_conquest = [
    '7g7f', '3c3d', '8h2b+', '3a2b', '7i8h', '8c8d', '8h7g', '5a4b',
    '3i3h', '4a3b', '1g1f', '7c7d', '4g4f', '1c1d', '9g9f', '9c9d',
    '3g3f', '7a7b', '2i3g', '8a7c', '4i4h', '8d8e', '6g6f', '6c6d',
    '2h2i', '6d6e', '6i7h', '6e6f', '5i5h', '8e8f', '8g8f', '3d3e',
    '3h4g', 'P*8h', '8i9g', '3e3f', '4g3f', 'P*3d', '9g8e', '7c8e',
    '7g6f', '2b3c', '3f4e', '8e9g+', '9i9g', '8b8f', 'B*6d', 'B*8g',
    '6d8f', '8g7h+', 'P*6b', '6a7a', '4e5d', 'N*4a', '3g4e', '3c4d',
    'N*6e', 'G*6c', '6e5c+', '4a5c', '4e5c+', '6c5c', 'P*3c', '2a3c',
    '5d5c', '4d5c', 'R*2a', 'N*3f', 'N*6e', '3f4h+', '5h4h', 'N*3f',
    '4h4g', 'P*6d', 'N*5d', '4b5b', '6f5e', 'S*5h', '4g3f', 'G*4e',
    '3f2f', '4e5e', '2a7a+', 'S*3e', '2f1g', '5e6e', '7a7b', '5c5d',
    '1g1h', '5d6c', '7b7c', 'N*8a'
]

gote_conquest = [
    '2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f',
    '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c',
    '5i5h', '5a5b', '3g3f', '8f7f', '8i7g', '3c5e', 'P*2b', '2a3c',
    '2b2a+', '3a4b', 'P*2h', 'P*2g', '4i3h', 'P*8g', '7h8g', '7f7d',
    '3d7d', '7c7d', '3h2g', '3c4e', 'R*3e', '4e5g+', '5h5g', 'P*3d',
    '3e5e', 'R*5i', '5g4f', '5i3i+', '7g6e', '7a6b', 'P*5d', 'S*4d',
    '5e2e', 'P*2d', '2e2d', '5c5d'
]

def format_moves(moves):
    lines = []
    chunk_size = 8
    for i in range(0, len(moves), chunk_size):
        chunk = moves[i:i+chunk_size]
        strs = [f'"{m}"' for m in chunk]
        line = "      , " + ", ".join(strs) if i > 0 else "      [ " + ", ".join(strs)
        lines.append(line)
    lines[-1] += "\n      ]"
    return "\n".join(lines)

sente_hs = format_moves(sente_conquest)
gote_hs = format_moves(gote_conquest)

with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

# baseBookMoves の先頭に挿入
target_header = "baseBookMoves :: [(String, [String])]\nbaseBookMoves =\n  ["

new_entry = f'''baseBookMoves :: [(String, [String])]
baseBookMoves =
  [ ( "Kakugawari_Suisho_Conquest"
{sente_hs}
    )
  , ( "Gote_Aigakari_Suisho_Conquest"
{gote_hs}
    )
  ,'''

# 既存の同名エントリがあれば置換、なければ挿入
if '("Kakugawari_Suisho_Conquest"' in content or '("Kakugawari_Suisho_Conquest"' in content.replace(' ', ''):
    # すでに Kakugawari_Suisho_Conquest があるので、baseBookMoves 全体を綺麗に書き換え
    start_idx = content.find("baseBookMoves :: [(String, [String])]")
    end_idx = content.find("    ( \"Aigakari_StratifiedEquilibrium\"", start_idx)
    if end_idx != -1:
        content = content[:start_idx] + new_entry + "\n" + content[end_idx:]
else:
    content = content.replace(target_header, new_entry, 1)

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("SUCCESS: Registered 92-ply Sente and 52-ply Gote Suisho Conquest Lines into GenerateStratifiedCompleteBook.hs!")
