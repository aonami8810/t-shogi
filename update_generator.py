# Update GenerateStratifiedCompleteBook.hs with authentic Nakabisha lines

with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    content = f.read()

# Replace GokigenNakabisha_StratifiedEquilibrium and AggressiveCentralRook_Melee
old_nakabisha_block = """  , ( "GokigenNakabisha_StratifiedEquilibrium"
    , [ "5g5f", "3c3d", "5h5e", "8c8d", "5e5f", "8d8e", "7g7f", "6b6c"
      , "7i6h", "4a3b", "4i5h", "5a4b", "3i4h", "3a2b", "5i4h", "4b3a"
      , "4h3h", "7a6b", "3h2h", "6b7c", "2g2f", "7c8d", "2f2e", "8e8f"
      , "8g8f", "8b8f", "P*8g", "8f8b", "3g3f", "8d7c", "S*7h"
      ]
    )
  , ( "SujichigaiBishop_Melee"
    , [ "7g7f", "3c3d", "8h2b+", "3a2b", "B*4e", "1c1d", "1g1f", "1d1e", "1f1e", "2a1c", "4e6c+", "1c2e", "6c4a", "5a4a", "G*4b", "8b4b", "1e1d", "G*5h", "2h5h", "2b3c", "1d1c+", "1a1b", "1c1b", "2e1g+", "1b1a", "1g1h", "1a2a", "1h1i", "G*3a", "4a5a", "2a1a", "1i2i", "1a2a", "2i3i", "2a1a" ]
    )
  , ( "Early3eBlitz_Melee"
    , [ "7g7f", "3c3d", "2g2f", "8c8d", "3g3f", "1c1d", "1g1f", "1d1e", "1f1e", "2a1c", "1e1d", "1c2e", "1d1c+", "1a1b", "1c1b", "2b3c", "1b1a", "2e1g+", "1a2a", "1g1h", "1i1h", "3a3b", "2a3a", "3b2a", "2f2e", "2a2b", "2e2d", "2b1c", "2d2c+", "1c1d", "2c2b", "1d1e", "2b2a", "1e1f", "2a1a" ]
    )
  , ( "AggressiveCentralRook_Melee"
    , [ "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "1c1d", "1g1f", "1d1e", "1f1e", "2a1c", "8h2b+", "1c2e", "1e1d", "2e1g+", "1d1c+", "1a1b", "1c1b", "1g1h", "1b1a", "1h1i", "1a2a", "1i2i", "2a3a", "2i3i", "2b3c", "4a4b", "3c4b", "5a4b", "G*3b", "4b5a", "S*4b", "5a5b", "2g2f", "3d3e", "2f2e" ]
    )"""

new_nakabisha_block = """  , ( "SenteNakabisha_IronMino_Equilibrium"
    , [ "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "7i7h", "4a3b"
      , "5f5e", "5a4b", "3i3h", "7a6b", "5i4h", "6c6d", "4h3i", "3a4a"
      , "3i2h", "4b3a", "4i3i", "6b6c", "6i6h", "8e8f", "8g8f", "8b8f"
      , "P*8g", "8f8b", "5h5f", "7c7d", "6h5h", "8a7c", "1g1f", "1c1d"
      , "5h5g", "7c6e", "7h6h", "6e7g+", "6h7g", "8b8d", "5f4f", "6c5d"
      , "5e5d", "6d6e", "7g6f"
      ]
    )
  , ( "GoteGokigen_IronDefense_Equilibrium"
    , [ "7g7f", "3c3d", "2g2f", "5c5d", "2f2e", "8b5b", "3g3f", "7a6b"
      , "3i3h", "6b5c", "3h3g", "5c4d", "3g4f", "4a3b", "2e2d", "2c2d"
      , "2h2d", "P*2c", "2d2h", "5a6b", "6i7h", "6b7b", "5i6h", "7b8b"
      , "4i4h", "6a7b", "1g1f", "1c1d", "2i3g", "4d5e", "4f5e", "5b5e"
      , "P*5f", "5e5a", "8h2b+", "3a2b", "B*5c", "5a5b", "5c4b+", "3b4b"
      , "2h2a+", "2b3a", "2a2c", "4b3b", "2c3b", "3a3b", "G*4a"
      ]
    )
  , ( "SenteKakuexchange_IronMino_Equilibrium"
    , [ "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "7i7h", "8e8f"
      , "8g8f", "8b8f", "P*8g", "8f8b", "8h2b+", "3a2b", "6i6h", "7a6b"
      , "5i4h", "5a4b", "3i3h", "4a3b", "4h3i", "4b3a", "3i2h", "5c5d"
      , "4i3i", "6b5c", "1g1f", "1c1d", "5f5e", "8b8d", "5h5g", "8d7d"
      , "6h5h", "7d8d", "5h4h", "8d8e", "3g3f", "8e5e", "5g5e", "5c4d"
      , "B*5c", "3b4b", "5c4b+", "3a4b", "G*5a"
      ]
    )
  , ( "SujichigaiBishop_Melee"
    , [ "7g7f", "3c3d", "8h2b+", "3a2b", "B*4e", "1c1d", "1g1f", "1d1e", "1f1e", "2a1c", "4e6c+", "1c2e", "6c4a", "5a4a", "G*4b", "8b4b", "1e1d", "G*5h", "2h5h", "2b3c", "1d1c+", "1a1b", "1c1b", "2e1g+", "1b1a", "1g1h", "1a2a", "1h1i", "G*3a", "4a5a", "2a1a", "1i2i", "1a2a", "2i3i", "2a1a" ]
    )
  , ( "Early3eBlitz_Melee"
    , [ "7g7f", "3c3d", "2g2f", "8c8d", "3g3f", "1c1d", "1g1f", "1d1e", "1f1e", "2a1c", "1e1d", "1c2e", "1d1c+", "1a1b", "1c1b", "2b3c", "1b1a", "2e1g+", "1a2a", "1g1h", "1i1h", "3a3b", "2a3a", "3b2a", "2f2e", "2a2b", "2e2d", "2b1c", "2d2c+", "1c1d", "2c2b", "1d1e", "2b2a", "1e1f", "2a1a" ]
    )"""

# Handle CRLF
is_crlf = '\r\n' in content
if is_crlf:
    content = content.replace('\r\n', '\n')

assert old_nakabisha_block in content, "old_nakabisha_block not found in GenerateStratifiedCompleteBook.hs"
content = content.replace(old_nakabisha_block, new_nakabisha_block, 1)

if is_crlf:
    content = content.replace('\n', '\r\n')

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("GenerateStratifiedCompleteBook.hs updated successfully with ironclad Nakabisha lines!")
