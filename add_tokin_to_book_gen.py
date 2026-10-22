with open("c:\\VS\\Workspace\\t-shogi\\GenerateStratifiedCompleteBook.hs", "r", encoding="utf-8") as f:
    content = f.read()

target = '        , "B*5c", "3b4b", "5c4b+", "3a4b", "G*5a"\n        ]\n      )\n'

new_entry = '''  , ( "SenteNakabisha_TokinAssault_Equilibrium"
    , [ "7g7f", "3c3d", "5g5f", "8c8d", "2h5h", "8d8e", "5f5e", "6c6d", "5e5d", "7a6b"
      , "5d5c+", "4a3b", "5c4b", "5a4b", "5i4h", "8e8f", "8g8f"
      ]
    )
  , ( "GoteGokigen_TokinAssault_Equilibrium"
    , [ "2g2f", "5c5d", "2f2e", "8b5b", "3g3f", "5d5e", "3i4h", "5e5f", "6i7h", "5f5g+"
      , "4i3h", "5g5h"
      ]
    )
'''

if target in content:
    content = content.replace(target, target + new_entry, 1)
    with open("c:\\VS\\Workspace\\t-shogi\\GenerateStratifiedCompleteBook.hs", "w", encoding="utf-8") as f:
        f.write(content)
    print("SUCCESS: Added tokin assault equilibrium lines to GenerateStratifiedCompleteBook.hs")
else:
    print("ERROR: Target string not found")
