# -*- coding: utf-8 -*-
import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

def test_replies(first_move, count=5):
    proc = subprocess.Popen(
        [SUISHO_EXE], cwd=SUISHO_DIR, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
        text=True, encoding="utf-8", errors="replace"
    )
    def send(c): proc.stdin.write(c + "\n"); proc.stdin.flush()
    def read_until(target):
        while True:
            l = proc.stdout.readline()
            if not l or target in l: break

    send("usi")
    read_until("usiok")
    send("isready")
    read_until("readyok")

    results = []
    for i in range(count):
        send("usinewgame")
        send(f"position startpos moves {first_move}")
        send("go btime 0 wtime 0 byoyomi 500")
        while True:
            l = proc.stdout.readline()
            if l.startswith("bestmove"):
                results.append(l.split()[1])
                break
    send("quit")
    proc.wait()
    return results

if __name__ == "__main__":
    print("Testing Suisho5 replies to 2g2f (5 times):")
    res_2g = test_replies("2g2f", 5)
    print("Replies to 2g2f:", res_2g)

    print("\nTesting Suisho5 replies to 7g7f (5 times):")
    res_7g = test_replies("7g7f", 5)
    print("Replies to 7g7f:", res_7g)
