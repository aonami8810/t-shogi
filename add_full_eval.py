with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

def find_line(pat, start=0):
    for i in range(start, len(lines)):
        if pat in lines[i]:
            return i
    return -1

idx_eval_cmd = find_line('"eval" -> do')
idx_put = find_line('putStrLn $ "info string eval total=', idx_eval_cmd)

detailed_eval = """        let p = turn currentState
            opp = opponent p
            mat = evalMaterial p currentState * wMaterial weights
            hang = evalHangingPieces p currentState
            atk = (evalMidgameAttack p currentState - evalMidgameAttack opp currentState) * wAttack weights
            defn = (evalMidgameDefense p currentState - evalMidgameDefense opp currentState) * wDefense weights
            ishida = evalSilverAdvanceAndIshidaFormation p currentState - evalSilverAdvanceAndIshidaFormation opp currentState
            ctrl = evalPieceControl p currentState * wPieceControl weights
            loose = evalLoosePieces p currentState * wLoosePenalty weights
            tempo = evalTempoLoss p currentState * wTempoLoss weights
            bdrop = evalBishopDropVulnerability p currentState - evalBishopDropVulnerability opp currentState
            potRay = evalPotentialRayAttack p currentState * wPotentialRays weights
            total = evaluateState weights p currentState
        putStrLn $ "info string eval total=" ++ show total ++ " hang=" ++ show hang ++ " loose=" ++ show loose ++ " tempo=" ++ show tempo ++ " bdrop=" ++ show bdrop ++ " potRay=" ++ show potRay ++ " ishida=" ++ show ishida
"""

lines[idx_eval_cmd+1:idx_put+1] = [l + '\n' for l in detailed_eval.strip().split('\n')]

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)

print("Saved full eval breakdown.")
