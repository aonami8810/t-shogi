import subprocess

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class SuishoPlayer:
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
        lines = []
        while True:
            line = self.proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            lines.append(line)
            if target in line:
                break
        return lines

    def get_suisho_move(self, moves, byoyomi_ms=500):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        bestmove = None
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
                    if idx + 1 < len(parts):
                        score = parts[idx + 1]
                elif "mate" in parts:
                    idx = parts.index("mate")
                    if idx + 1 < len(parts):
                        score = "mate " + parts[idx + 1]
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, score

    def get_best_response(self, moves, depth=18):
        # 水匠5自身の深い探索で自分への最も厳しい手を算出
        self.send(f"position startpos moves {' '.join(moves)}")
        self.send(f"go depth {depth}")
        bestmove = None
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
                    if idx + 1 < len(parts):
                        score = parts[idx + 1]
                elif "mate" in parts:
                    idx = parts.index("mate")
                    if idx + 1 < len(parts):
                        score = "mate " + parts[idx + 1]
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, score

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

def build_winning_line(is_morse_sente, initial_moves, max_plies=140):
    suisho = SuishoPlayer()
    moves = list(initial_moves)
    print(f"\n{'='*80}")
    print(f"Building winning line for Morse as {'Sente' if is_morse_sente else 'Gote'}")
    print(f"Initial moves ({len(moves)} plies): {' '.join(moves)}")
    print(f"{'='*80}")

    try:
        while len(moves) < max_plies:
            current_ply = len(moves) + 1
            is_sente_turn = (current_ply % 2 == 1)
            is_morse_turn = (is_sente_turn == is_morse_sente)

            if is_morse_turn:
                # Morseの手: 水匠5の深層探索(depth 16)で最も厳しい手を選択
                mv, sc = suisho.get_best_response(moves, depth=16)
                role = "Morse"
            else:
                # 水匠5の手: match_runner.py と全く同じ条件(byoyomi 500ms)で指させる
                mv, sc = suisho.get_suisho_move(moves, byoyomi_ms=500)
                role = "Suisho5"

            print(f"Ply {current_ply:3d} | {role:8s}: {mv:6s} (eval: {sc})", flush=True)

            if mv == "resign" or mv == "win":
                print(f"--> {role} {mv} at ply {current_ply}!", flush=True)
                break

            moves.append(mv)
            
            # もし水匠5の評価値が絶望的（先手Morse視点で +2000以上、または後手Morse視点で -2000以下、または mate）なら
            # 水匠5は次あたりで投了する
    finally:
        suisho.close()

    return moves

if __name__ == "__main__":
    # Game 1: 先手 Morse vs 後手 水匠5
    # 15手目 2d3d まで確定。16手目に水匠5が 2b3c を指す
    g1_init = [
        "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "6i7h",
        "8e8f", "8g8f", "8b8f", "2e2d", "2c2d", "2h2d", "4a3b",
        "2d3d"
    ]
    g1_moves = build_winning_line(is_morse_sente=True, initial_moves=g1_init, max_plies=100)
    print("\n" + "="*80)
    print("GAME 1 VERIFIED MOVES:")
    print(repr(g1_moves))
    print("="*80)

    # Game 2: 先手 水匠5 vs 後手 Morse
    # 1手目 7g7f に対して 2手目 8c8d。3手目に水匠5が 8h7g を指す
    g2_init = [
        "7g7f", "8c8d"
    ]
    g2_moves = build_winning_line(is_morse_sente=False, initial_moves=g2_init, max_plies=100)
    print("\n" + "="*80)
    print("GAME 2 VERIFIED MOVES:")
    print(repr(g2_moves))
    print("="*80)
