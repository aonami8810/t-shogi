import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def test_ply2():
    for i in range(5):
        p = subprocess.Popen([SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
        def send(cmd): p.stdin.write(cmd + "\n"); p.stdin.flush()
        send("usi")
        while True:
            if "usiok" in p.stdout.readline(): break
        send("isready")
        while True:
            if "readyok" in p.stdout.readline(): break
        send("usinewgame")
        send("position startpos moves 7g7f")
        send("go btime 0 wtime 0 byoyomi 500")
        
        bm = None
        sc = None
        while True:
            l = p.stdout.readline().strip()
            if l.startswith("info") and "score" in l:
                sc = l
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        print(f"Run {i+1}: 7g7f -> Suisho5 plays: {bm} ({sc})")
        send("quit")
        p.kill()

if __name__ == "__main__":
    test_ply2()
