import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def analyze(moves_str, multipv=5, byoyomi_ms=5000):
    p = subprocess.Popen(
        [SUISHO_EXE],
        cwd=SUISHO_DIR,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace"
    )
    def send(c):
        p.stdin.write(c + "\n")
        p.stdin.flush()
    def read_until(t):
        while True:
            l = p.stdout.readline()
            if not l: break
            if t in l: break

    send("usi")
    read_until("usiok")
    send(f"setoption name MultiPV value {multipv}")
    send("isready")
    read_until("readyok")
    send(f"position startpos moves {moves_str}")
    send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")

    lines = []
    while True:
        l = p.stdout.readline()
        if not l: break
        l = l.strip()
        if l.startswith("info") and "multipv" in l and "pv" in l:
            lines.append(l)
        if l.startswith("bestmove"):
            break
    send("quit")
    p.wait()
    return lines[-multipv:]

if __name__ == "__main__":
    pos_gote = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8i7g 3c5e P*2b 2a3c 2b2a+ 3a4b 3d8d 5e1i+ 8d8a+ 3c4e 8a8e"
    print("=== MultiPV for Gote at Ply 32 ===")
    results = analyze(pos_gote, multipv=5, byoyomi_ms=6000)
    for r in results:
        print(r)
