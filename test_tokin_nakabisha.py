import subprocess

def run_usi_moves(moves_str):
    p = subprocess.Popen(
        ["c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse.exe"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8"
    )
    def send(c):
        p.stdin.write(c + "\n")
        p.stdin.flush()
    def read(t):
        lines = []
        while True:
            l = p.stdout.readline()
            if not l: break
            lines.append(l.strip())
            if t in l: break
        return lines
    
    send("usi")
    read("usiok")
    send("isready")
    read("readyok")
    send(f"position startpos moves {moves_str}")
    send("go depth 3")
    lines = read("bestmove")
    send("quit")
    p.wait()
    
    bm = None
    pv = None
    for l in lines:
        if "bestmove" in l:
            bm = l.split()[1]
        elif "info depth" in l:
            pv = l
    return bm, pv

# 局面1: 後手中飛車が 5七歩成（5f5g+）と先手居玉（5九玉）に成り込んだ直後
# 先手が 4九金 4i4h または 6九金 6i5h と応じた後の、後手の指し手
print("=== CASE 1: 後手中飛車の5七歩成（先手居玉5九） ===")
# 7g7f 3c3d 2g2f 5c5d 2f2e 5d5e 2e2d 2c2d 2h2d 8b5b 5i4h (居玉回避) ではなく居玉のまま
# 相手が居玉のパターン:
# 1. 2g2f 5c5d 2f2e 8b5b 5g5f 5d5e 6g6f 5e5f 6i7h 5f5g+ (5七歩成) 4i3h (先手居玉のまま)
moves_gote = "2g2f 5c5d 2f2e 8b5b 3g3f 5d5e 3i4h 5e5f 6i7h 5f5g+ 4i3h"
bm1, pv1 = run_usi_moves(moves_gote)
print("Position:", moves_gote)
print("PV:", pv1)
print("Bestmove for Gote:", bm1)

print("\n=== CASE 2: 先手中飛車が 5三歩成（5e5c+ / 5d5c+）と後手居玉（5一玉）に成り込んだ直後 ===")
# 先手中飛車で 5三歩成
# 7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 5f5e 4a3b 5e5d 5a4b ではなく居玉 6c6d 5d5c+ 4a3b
moves_sente = "7g7f 3c3d 5g5f 8c8d 2h5h 8d8e 5f5e 6c6d 5e5d 7a6b 5d5c+ 4a3b"
bm2, pv2 = run_usi_moves(moves_sente)
print("Position:", moves_sente)
print("PV:", pv2)
print("Bestmove for Sente:", bm2)
