import subprocess

def evaluate_move(moves_so_far, candidate_move):
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
    full_moves = f"{moves_so_far} {candidate_move}"
    send(f"position startpos moves {full_moves}")
    send("go depth 1")
    lines = read("bestmove")
    send("quit")
    p.wait()
    for l in lines:
        if "info depth" in l:
            print(f"Candidate {candidate_move} -> child search: {l}")

moves = "7g7f 8c8d 2g2f 8d8e 2f2e 8e8f"
print("Evaluating moves from 8e8f:")
evaluate_move(moves, "8g8f")
evaluate_move(moves, "5i6h")
