import subprocess

def send_cmd(p, cmd):
    p.stdin.write(cmd + "\n")
    p.stdin.flush()

def read_until(p, target):
    lines = []
    while True:
        line = p.stdout.readline()
        if not line:
            break
        lines.append(line.strip())
        if target in line:
            break
    return lines

def test_scenario(title, moves_str, expected_moves):
    print("=" * 80)
    print(f"TEST: {title}")
    print("=" * 80)
    p = subprocess.Popen(
        ["c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse.exe"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8"
    )
    send_cmd(p, "usi")
    read_until(p, "usiok")
    send_cmd(p, "isready")
    read_until(p, "readyok")
    
    pos_cmd = f"position startpos moves {moves_str}" if moves_str else "position startpos"
    send_cmd(p, pos_cmd)
    send_cmd(p, "go depth 2")
    lines = read_until(p, "bestmove")
    send_cmd(p, "quit")
    p.wait()
    
    bestmove = None
    pv = None
    for line in lines:
        if "bestmove" in line:
            bestmove = line.split()[1]
        elif "info depth" in line:
            pv = line
            
    print(f"Moves so far: {moves_str}")
    print(f"PV: {pv}")
    print(f"Bestmove: {bestmove}")
    success = bestmove in expected_moves
    print(f"Expected one of {expected_moves}: {'PASS' if success else 'FAIL'}")
    print()
    return success

# 1. 居飛車相掛かり調：8六歩に対する同歩 (8g8f)
test_scenario("A1: 居飛車飛車先歩突き (8六歩) に対する同歩 (8g8f)", 
              "7g7f 8c8d 2g2f 8d8e 2f2e 8e8f", 
              ["8g8f"])

# 2. 居飛車相掛かり調：8六同飛に対する歩打ち受け (P*8g)
test_scenario("A2: 居飛車飛車先突破 (8六同飛) に対する歩打ち受け (P*8g)", 
              "7g7f 8c8d 2g2f 8d8e 2f2e 8e8f 8g8f 8b8f", 
              ["P*8g"])

# 3. 角交換中飛車：8六歩に対する同歩 (8g8f)
test_scenario("B1: 角交換中飛車：8六歩に対する同歩 (8g8f)", 
              "7g7f 3c3d 8h2b+ 3a2b 5g5f 8c8d 2h5h 8d8e 7i7h 8e8f", 
              ["8g8f"])

# 4. 角交換中飛車：8六同飛に対する歩打ち受け (P*8g)
test_scenario("B2: 角交換中飛車：8六同飛に対する歩打ち受け (P*8g)", 
              "7g7f 3c3d 8h2b+ 3a2b 5g5f 8c8d 2h5h 8d8e 7i7h 8e8f 8g8f 8b8f", 
              ["P*8g"])

# 5. 角交換中飛車受身完了後：居玉脱出・美濃囲い or つの銀への展開 (5i4h or 3i4h or 6i5h or 7i6h)
test_scenario("B3: 角交換中飛車受身完了後：居玉脱出・美濃囲い・つの銀展開", 
              "7g7f 3c3d 8h2b+ 3a2b 5g5f 8c8d 2h5h 8d8e 7i7h 8e8f 8g8f 8b8f P*8g 8f8b", 
              ["5i4h", "3i4h", "3i3h", "6i5h", "7i6h"])

# 6. 先手ノーマル中飛車：美濃囲い初手 (5i4h)
test_scenario("C1: 先手中飛車：美濃囲い初手 (5i4h)", 
              "7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 7i7h 4a3b", 
              ["5i4h", "5f5e", "3i3h"])
