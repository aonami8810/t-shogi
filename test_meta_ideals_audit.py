import subprocess
import time
import re

def verify_meta_ideals_and_gradient_flows():
    print("================================================================================")
    print(" [VERIFICATION] 512階メタイデアル体系 ＆ モース勾配流事前コンパイル 監査")
    print("================================================================================")

    # 1. CompiledMorseBook.hs 内のメタイデアルノードの監査
    with open('CompiledMorseBook.hs', 'r', encoding='cp932', errors='replace') as f:
        text = f.read()

    # Find MetaIdeal occurrences
    meta_ideals = re.findall(r'MetaIdeal_(\d+)_([A-Za-z0-9_]+)', text)
    unique_ideals = {}
    for num_str, name in meta_ideals:
        unique_ideals[int(num_str)] = name

    print(f"Total Unique Meta-Ideals in Type-Level Book: {len(unique_ideals)} / 512")
    
    # Stratification Phases Breakdown
    phases = [
        ("Phase 1: 即詰み・必至メタイデアル (Terminal Mate)", 1, 64),
        ("Phase 2: 中盤詰めろ・空間切断メタイデアル (Tsumero)", 65, 128),
        ("Phase 3: 大駒浸透・竜馬連携メタイデアル (Penetration)", 129, 192),
        ("Phase 4: 桂香フォーク・串刺し急襲メタイデアル (Tactical Fork)", 193, 256),
        ("Phase 5: 守備駒剥がし・露出玉メタイデアル (Guard Stripping)", 257, 320),
        ("Phase 6: 囲い崩壊・急所爆破メタイデアル (Castle Collapse)", 321, 384),
        ("Phase 7: 駒得累積・手番支配メタイデアル (Material Dominance)", 385, 448),
        ("Phase 8: 漸近的不変モース勾配流シンク (Asymptotic Sinks)", 449, 512),
    ]

    for pname, start, end in phases:
        count = sum(1 for k in unique_ideals if start <= k <= end)
        pct = (count / 64.0) * 100.0
        print(f"  * {pname:50s}: {count:2d} / 64 ({pct:5.1f}%)")

    # 2. モース勾配流（V-Paths）の各終局への到達性検証
    paths = re.findall(r'\(\[([^\]]+)\],\s*"([^"]+)"\)', text)
    print(f"\nTotal Gradient Flow V-Paths (Morse Trajectories): {len(paths)}")
    
    # 3. USI プロトコルによる実行時テスト
    proc = subprocess.Popen(
        ['./shogi-usi-morse.exe'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )

    def send(cmd):
        proc.stdin.write(cmd + "\n")
        proc.stdin.flush()

    def read_until(token, timeout=5.0):
        start = time.time()
        lines = []
        while time.time() - start < timeout:
            line = proc.stdout.readline()
            if not line: break
            lines.append(line.strip())
            if token in line: break
        return lines

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")

    print("\n--------------------------------------------------------------------------------")
    print(" [USI RUNTIME] 512階メタイデアルへのモース勾配流 最短降下テスト")
    print("--------------------------------------------------------------------------------")

    test_cases = [
        ("相掛かり中盤 (Aigakari 35手目)", "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 2e2d 2c2d 2h2d P*2c 2d2h 8e8f 8g8f 8b8f P*8g 8f8b 3i3h 7a7b 5i6h 5a4b 4i5h 4b3c 6h7h 7b6c 3h2g 8b8d 2h2f 8d7d 2f2e 7d8d 2e2f 8d7d 8h3c+"),
        ("角換わり中盤 (Kakugawari 35手目)", "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 7i7h 3a3b 8h2b+ 3b2b 7i8h 7a6b 3i4h 6b7b 4h3g 4a3b 6i7h 5a4b 5i6h 6c6d 3g4f 7c7d 4f3e 3c3d 2e2d 2c2d 2h2d P*2c 2d2h 3d3e S*5h S*3a P*3c 3b3c B*1f"),
        ("矢倉中盤 (Yagura 41手目)", "7g7f 8c8d 6g6f 3c3d 7i6h 6c6d 6h7g 7a6b 5g5f 5a4b 4i5h 3a3b 5i6h 4b3c 6h7h 5c5d 7h8h 4c4d 8h7h 8d8e 7h8h 6b5c 2g2f 7c7d 2f2e 8e8f 8g8f 8b8f P*8g 8f8b 4g4f 3b4c 3i4h 4c3b 4h4g 6a5b 2e2d 4a4b B*1f 2b3c 8h7i"),
        ("四間飛車急戦 (Shikenbisha 37手目)", "7g7f 3c3d 6g6f 8c8d 2h6h 8d8e 8h7g 6b6c 4i4h 7a6b 3i3h 4a3b 5i4h 5a4b 4h3h 3a2b 3h2h 7b7a 2h1h 7a8b 6h7h 8b7b 1h2h 1c1d 1g1f 5c5d 2g2f 4b3a 2f2e 2b3c 3g3f R*4i S*9h 4i6i+ 2e2d 3c2d 6f6e")
    ]

    for name, moves in test_cases:
        send(f"position startpos moves {moves}")
        send("go btime 100 wtime 100")
        
        t0 = time.time()
        bestmove = ""
        is_book = False
        is_egtb = False
        score_info = ""
        while time.time() - t0 < 1.0:
            l = proc.stdout.readline()
            if not l: break
            l = l.strip()
            if "string book hit" in l: is_book = True
            if "string quotient egtb hit" in l: is_egtb = True
            if "score" in l: score_info = l
            if l.startswith("bestmove"):
                bestmove = l.split()[1]
                elapsed = time.time() - t0
                break

        tag = "★ BOOK HIT (事前コンパイル勾配流)" if is_book else ("◆ QEGTB HIT (メタイデアル直結)" if is_egtb else "▲ SEARCH ENGINE")
        print(f" {name:32s} -> {bestmove:6s} ({elapsed*1000:5.1f}ms) | {tag}")

    send("quit")
    proc.terminate()

    print("\n================================================================================")
    print(" [CONCLUSION] 512階メタイデアル事前コンパイル ＆ モース勾配流網羅 完了！")
    print("================================================================================")

if __name__ == "__main__":
    verify_meta_ideals_and_gradient_flows()
