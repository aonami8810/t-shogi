file_path = "c:/VS/Workspace/t-shogi/GenerateStratifiedCompleteBook.hs"
with open(file_path, "r", encoding="utf-8") as f:
    content = f.read()

# 1. Change maxDepth from 512 to 15 for fast closure without stalling
content = content.replace("searchPunishmentIDS512 devSt qAcc 512", "searchPunishmentIDS512 devSt qAcc 15")
content = content.replace("searchPunishmentIDS512 lastSt updatedQEGTB 512", "searchPunishmentIDS512 lastSt updatedQEGTB 15")

# 2. In searchBounded, also check detectMidgameTsumero for instant snap to nearest ideal
target_match = """      searchBounded s q 0 accMoves _ =
        case matchQEGTB s q of
          Just (_, mateM) -> (reverse (mateM : accMoves), True, q)
          Nothing          -> (reverse accMoves, False, q)
      searchBounded s q remDepth accMoves visited
        | isCheckmatePosition s =
            let dummyMv = DropMove Gold (5, 2)
                newQ = extractAndBackpropagateQEGTB s dummyMv 0 q
            in (reverse accMoves, True, newQ)
        | otherwise =
            case matchQEGTB s q of
              Just (_, mateM) -> (reverse (mateM : accMoves), True, q)
              Nothing ->"""

replacement_match = """      searchBounded s q 0 accMoves _ =
        case matchQEGTB s q of
          Just (_, mateM) -> (reverse (mateM : accMoves), True, q)
          Nothing ->
            case detectMidgameTsumero (turn s) s q of
              Just (_, tsumeroM) -> (reverse (tsumeroM : accMoves), True, q)
              Nothing            -> (reverse accMoves, False, q)
      searchBounded s q remDepth accMoves visited
        | isCheckmatePosition s =
            let dummyMv = DropMove Gold (5, 2)
                newQ = extractAndBackpropagateQEGTB s dummyMv 0 q
            in (reverse accMoves, True, newQ)
        | otherwise =
            case matchQEGTB s q of
              Just (_, mateM) -> (reverse (mateM : accMoves), True, q)
              Nothing ->
                case detectMidgameTsumero (turn s) s q of
                  Just (_, tsumeroM) -> (reverse (tsumeroM : accMoves), True, q)
                  Nothing ->"""

content = content.replace(target_match, replacement_match)

with open(file_path, "w", encoding="utf-8", newline="\n") as f:
    f.write(content)

print("Optimized book generator to snap to nearest ideals (QEGTB + MTI) at maxDepth 15!")
