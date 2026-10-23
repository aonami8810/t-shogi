# -*- coding: utf-8 -*-

with open("conquest_code.txt", "r", encoding="utf-8") as f:
    conquest_code = f.read()

with open("ShogiUsiMorse-v4.hs", "r", encoding="utf-8") as f:
    content = f.read()

target = """-- | 現在の指し手履歴から、CompiledMorseBook の V-Path に適合する定跡手を検索する
lookupBookMove :: [String] -> State -> Maybe Move
lookupBookMove currentMoves st =
  let legalMoves = f5SyzygyFilter st (generateMoves st)"""

replacement = f"""{conquest_code}
-- | 現在の指し手履歴から、CompiledMorseBook の V-Path に適合する定跡手を検索する
lookupBookMove :: [String] -> State -> Maybe Move
lookupBookMove currentMoves st =
  case lookupDirectConquestMove currentMoves st of
    Just m -> Just m
    Nothing ->
      let legalMoves = f5SyzygyFilter st (generateMoves st)"""

assert target in content, "Target not found in ShogiUsiMorse-v4.hs!"

new_content = content.replace(target, replacement, 1)

with open("ShogiUsiMorse-v4.hs", "w", encoding="utf-8") as f:
    f.write(new_content)

print("Successfully patched ShogiUsiMorse-v4.hs!")
