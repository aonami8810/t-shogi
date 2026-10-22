file_path = "c:/VS/Workspace/t-shogi/GenerateStratifiedCompleteBook.hs"
with open(file_path, "r", encoding="utf-8") as f:
    content = f.read()

old_block = """          -- 逸脱手をIDS探索で最速論駁してQEGTBへ接続
          let (branchPaths, finalQEGTB) = foldl' (\\(bP, qAcc) devM ->
                let devSt = applyMove devM st
                    (refutedMvSeq, qegtbHit, newQ) = searchPunishmentIDS512 devSt qAcc 512
                    fullSeq = take idx moves ++ [devM] ++ refutedMvSeq
                    branchName = printf "%s_DevPly%02d_%s" pathName (idx + 1) (showMove devM)
                    stratumAtDev = showStratumKey (stratifyState devSt)
                    pathObj = BFSPath branchName fullSeq qegtbHit (length refutedMvSeq) stratumAtDev
                in (pathObj `seq` (pathObj : bP), newQ `seq` newQ)
                ) ([], curQEGTB) (take 2 deviatingMoves)
          
          return (accP ++ branchPaths, finalQEGTB)
          ) ([], qegtbAcc) (zip [0..] states)
        
        let equilibriumPath = BFSPath pathName moves True 0 sampleStratum
        putStrLn $ printf "    Refuted %d Deviating Branches connecting directly to QEGTB!" (length newRefutedPaths)
        return (equilibriumPath : newRefutedPaths ++ pathsAcc, updatedQEGTB, uniqueStrata ++ strataAcc)"""

new_block = """          -- 逸脱手をIDS探索で最速論駁してQEGTBへ接続（30手目以降はさらに分厚く分岐を網羅）
          let branchBudget = if idx >= 24 then 4 else 2
          let (branchPaths, finalQEGTB) = foldl' (\\(bP, qAcc) devM ->
                let devSt = applyMove devM st
                    (refutedMvSeq, qegtbHit, newQ) = searchPunishmentIDS512 devSt qAcc 512
                    fullSeq = take idx moves ++ [devM] ++ refutedMvSeq
                    branchName = printf "%s_DevPly%02d_%s" pathName (idx + 1) (showMove devM)
                    stratumAtDev = showStratumKey (stratifyState devSt)
                    pathObj = BFSPath branchName fullSeq qegtbHit (length refutedMvSeq) stratumAtDev
                in (pathObj `seq` (pathObj : bP), newQ `seq` newQ)
                ) ([], curQEGTB) (take branchBudget deviatingMoves)
          
          return (accP ++ branchPaths, finalQEGTB)
          ) ([], qegtbAcc) (zip [0..] states)
        
        -- ★ 本筋自身を30手目以降〜終局・最近傍イデアル（QEGTB詰み）まで完全延伸！
        let lastSt = last states
            (extendedMoves, isEquilClosed, qegtbAfterEquil) =
              searchPunishmentIDS512 lastSt updatedQEGTB 512
            fullEquilibriumMoves = moves ++ extendedMoves
            equilibriumPath = BFSPath pathName fullEquilibriumMoves isEquilClosed (length extendedMoves) sampleStratum
        
        putStrLn $ printf "    Extended Equilibrium Line by %d plies to Nearest QEGTB Ideal!" (length extendedMoves)
        putStrLn $ printf "    Refuted %d Deviating Branches connecting directly to QEGTB!" (length newRefutedPaths)
        return (equilibriumPath : newRefutedPaths ++ pathsAcc, qegtbAfterEquil, uniqueStrata ++ strataAcc)"""

content = content.replace(old_block, new_block)

with open(file_path, "w", encoding="utf-8", newline="\n") as f:
    f.write(content)

print("Upgraded GenerateStratifiedCompleteBook.hs with Deep Equilibrium Extension and 4x Midgame Branch Coverage!")
