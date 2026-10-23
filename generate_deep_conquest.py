import subprocess
import sys

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class Engine:
    def __init__(self):
        self.proc = subprocess.Popen(
            [SUISHO_EXE],
            cwd=SUISHO_DIR,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            encoding="utf-8",
            errors="replace"
        )
        self.send("usi")
        self.read_until("usiok")
        self.send("isready")
        self.read_until("readyok")

    def send(self, cmd):
        self.proc.stdin.write(cmd + "\n")
        self.proc.stdin.flush()

    def read_until(self, target):
        while True:
            line = self.proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if target in line:
                break

    def bestmove(self, moves, byoyomi_ms=2500):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")

        bm = None
        score = None
        while True:
            line = self.proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if line.startswith("info") and "score" in line and "pv" in line:
                parts = line.split()
                if "cp" in parts:
                    idx = parts.index("cp")
                    score = int(parts[idx + 1])
                elif "mate" in parts:
                    idx = parts.index("mate")
                    mate_plies = parts[idx + 1]
                    score = 30000 if not mate_plies.startswith("-") else -30000
            if line.startswith("bestmove"):
                bm = line.split()[1]
                break
        return bm, score

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

def build_winning_line(name, initial_moves, target_winner="Sente", max_plies=75, byoyomi_ms=2500):
    print(f"\n=======================================================", flush=True)
    print(f"Generating Winning Line: {name} (Target: {target_winner})", flush=True)
    print(f"=======================================================", flush=True)
    eng = Engine()
    moves = list(initial_moves)
    
    try:
        while len(moves) < max_plies:
            ply = len(moves) + 1
            is_sente_turn = (ply % 2 == 1)
            turn_str = "Sente" if is_sente_turn else "Gote"
            
            # 探索時間を勝者側は長め（深層探索）、敗者側は通常（水匠5の実戦手）
            cur_byo = byoyomi_ms if ((turn_str == target_winner)) else 1000
            bm, score = eng.bestmove(moves, byoyomi_ms=cur_byo)
            
            # 手番から見た勝敗スコア
            # Sente視点: +ならSente有利, -ならGote有利
            sente_score = score if is_sente_turn else (-score if score is not None else 0)
            
            print(f"Ply {ply:3d} ({turn_str:5s}): {bm:6s} | SenteScore: {sente_score}", flush=True)
            
            if bm in ("resign", "win"):
                print(f"Finished at ply {ply}: {bm}", flush=True)
                break
                
            moves.append(bm)
            
        print(f"\nLine generated ({len(moves)} plies):")
        print(f"moves = {moves}")
        return moves
    finally:
        eng.close()

if __name__ == "__main__":
    # 1. Game 1: 先手番で 8手目 9c9d に対する 75手勝勢ライン
    init1 = "7g7f 3c3d 8h2b+ 3a2b 7i8h 2b3c 8h7g 9c9d".split()
    line1 = build_winning_line("Kakugawari_Suisho_Conquest_2b3c_9c9d_1g1f", init1, target_winner="Sente", max_plies=75, byoyomi_ms=2500)
    
    # 2. Game 2: 後手番で 31手目 8a8e に対する 75手勝勢ライン
    init2 = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8i7g 3c5e P*2b 2a3c 2b2a+ 3a4b 3d8d 5e1i+ 8d8a+ 3c4e 8a8e".split()
    line2 = build_winning_line("Gote_Aigakari_Suisho_Conquest_8a8e_L5e", init2, target_winner="Gote", max_plies=75, byoyomi_ms=2500)

    with open("new_conquest_lines.py", "w", encoding="utf-8") as f:
        f.write(f"line1 = {line1}\n")
        f.write(f"line2 = {line2}\n")
    print("\nSaved lines to new_conquest_lines.py")
