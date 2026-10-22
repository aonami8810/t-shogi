with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

def find_line(pat, start=0):
    for i in range(start, len(lines)):
        if pat in lines[i]:
            return i
    return -1

# Add eval command to usiLoop
idx_quit = find_line('"quit" -> do')
print(f"quit command at line {idx_quit + 1}")

eval_code = """      "eval" -> do
        let p = turn currentState
            opp = opponent p
            mat = evalMaterial p currentState
            atk = evalMidgameAttack p currentState - evalMidgameAttack opp currentState
            defn = evalMidgameDefense p currentState - evalMidgameDefense opp currentState
            ishida = evalSilverAdvanceAndIshidaFormation p currentState - evalSilverAdvanceAndIshidaFormation opp currentState
            ctrl = evalPieceControl p currentState
            total = evaluateState weights p currentState
        putStrLn $ "info string eval total=" ++ show total ++ " mat=" ++ show mat ++ " atk=" ++ show atk ++ " def=" ++ show defn ++ " ishida=" ++ show ishida ++ " ctrl=" ++ show ctrl
        hFlush stdout
        usiLoop weights currentState

"""

lines.insert(idx_quit, eval_code)
print("Added eval command to usiLoop.")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)

print("Saved ShogiUsiMorse-v4.hs")
