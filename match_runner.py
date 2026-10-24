import subprocess
import time
import sys

MORSE_EXE = r"c:\VS\Workspace\t-shogi\shogi-usi-morse.exe"
MORSE_DIR = r"c:\VS\Workspace\t-shogi"
SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_AVX2_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-avx2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"


class SubSolverTestHarness:
    """
    オフライン検証・定跡軌道生成用の Python テストハーネス。
    実戦用の純粋モースエンジン (ShogiUsiMorse-v7.hs) から分離された
    サブソルバ呼び出し（AVX2・12スレッド・1024MB Hash・DrawValue=-25）を管理する。
    """
    def __init__(self, exe_path=SUISHO_AVX2_EXE, cwd=SUISHO_DIR, threads=12, hash_mb=1024, draw_value=-25):
        self.exe_path = exe_path
        self.cwd = cwd
        self.threads = threads
        self.hash_mb = hash_mb
        self.draw_value = draw_value
        self.proc = subprocess.Popen(
            [exe_path],
            cwd=cwd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            encoding="utf-8",
            errors="replace"
        )
        self._init_solver()

    def _send(self, cmd):
        self.proc.stdin.write(cmd + "\n")
        self.proc.stdin.flush()

    def _read_until(self, target):
        while True:
            ln = self.proc.stdout.readline()
            if not ln:
                break
            if target in ln.strip():
                break

    def _init_solver(self):
        self._send("usi")
        self._read_until("usiok")
        self._send(f"setoption name Threads value {self.threads}")
        self._send(f"setoption name USI_Hash value {self.hash_mb}")
        self._send(f"setoption name DrawValueBlack value {self.draw_value}")
        self._send(f"setoption name DrawValueWhite value {self.draw_value}")
        self._send("isready")
        self._read_until("readyok")
        self._send("usinewgame")

    @staticmethod
    def select_subsolver_nodes(byoyomi_ms=5000):
        if byoyomi_ms < 1500:
            return 3000000
        elif byoyomi_ms < 3500:
            return 10000000
        return 16000000

    def query_subsolver(self, moves, byoyomi_ms=5000, allowed_root_moves=None):
        node_budget = self.select_subsolver_nodes(byoyomi_ms)
        pos_cmd = f"position startpos moves {' '.join(moves)}" if moves else "position startpos"
        if allowed_root_moves:
            re_nodes = max(2000000, min(4000000, node_budget // 3))
            go_cmd = f"go nodes {re_nodes} searchmoves {' '.join(allowed_root_moves)}"
        else:
            go_cmd = f"go nodes {node_budget}"

        self._send(pos_cmd)
        self._send(go_cmd)

        bestmove = None
        last_info = None
        score = None
        while True:
            ln = self.proc.stdout.readline()
            if not ln:
                break
            ln = ln.strip()
            ws = ln.split()
            if ln.startswith("info") and "score" in ws and "pv" in ws:
                last_info = ln
                if "cp" in ws:
                    idx = ws.index("cp")
                    if idx + 1 < len(ws):
                        score = ws[idx + 1]
                elif "mate" in ws:
                    idx = ws.index("mate")
                    if idx + 1 < len(ws):
                        score = "mate " + ws[idx + 1]
            elif ln.startswith("bestmove"):
                bestmove = ws[1]
                break
        return bestmove, score, last_info

    def close(self):
        try:
            self._send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

class UsiEngine:
    def __init__(self, name, exe_path, cwd):
        self.name = name
        self.exe_path = exe_path
        self.cwd = cwd
        self.proc = subprocess.Popen(
            [exe_path],
            cwd=cwd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            encoding="utf-8",
            errors="replace"
        )
        self.init_usi()

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

    def init_usi(self):
        self.send("usi")
        self.read_until("usiok")
        self.send("isready")
        self.read_until("readyok")
        self.send("usinewgame")

    def go(self, moves, byoyomi_ms=1000):
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")
        
        bestmove = None
        pv = None
        score = None
        is_book = False
        
        while True:
            line = self.proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if "book hit" in line:
                is_book = True
            if line.startswith("info") and "score" in line and "pv" in line:
                pv = line
                # スコア抽出
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
                
        return bestmove, score, pv, is_book

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

def run_game(sente_name, sente_engine, gote_name, gote_engine, max_plies=256, byoyomi_ms=1000):
    print("=" * 80, flush=True)
    print(f"  MATCH: [先手] {sente_name} vs [後手] {gote_name}", flush=True)
    print("=" * 80, flush=True)
    
    moves = []
    winner = None
    reason = None
    
    for ply in range(1, max_plies + 1):
        is_sente_turn = (ply % 2 == 1)
        current_name = sente_name if is_sente_turn else gote_name
        current_engine = sente_engine if is_sente_turn else gote_engine
        opp_name = gote_name if is_sente_turn else sente_name
        
        mv, score, pv, is_book = current_engine.go(moves, byoyomi_ms=byoyomi_ms)
        
        book_tag = " [BOOK]" if is_book else ""
        score_tag = f" (eval: {score})" if score else ""
        print(f"Ply {ply:3d} | {current_name:12s}: {mv:6s}{book_tag}{score_tag}", flush=True)
        
        if mv == "resign":
            winner = opp_name
            reason = f"{current_name} resigned"
            break
        elif mv == "win":
            winner = current_name
            reason = f"{current_name} declared victory (Jishogi)"
            break
        
        moves.append(mv)
        
        # 3回同一局面（簡易千日手チェック）
        # ここでは最大手数到達で引き分け判定
        if ply == max_plies:
            reason = f"Max plies reached ({max_plies})"
            winner = "Draw"
            break
            
    print("-" * 80, flush=True)
    print(f"RESULT: Winner = {winner} | Reason: {reason}", flush=True)
    print(f"Total plies: {len(moves)}", flush=True)
    print(f"Moves: {' '.join(moves)}", flush=True)
    print("=" * 80, flush=True)
    
    return winner, moves, reason

if __name__ == "__main__":
    byoyomi = 500 # 初回テストは500ms（高速対戦）
    
    # Game 1: 先手 Morse vs 後手 水匠5
    print("\n--- Starting Game 1: Morse (先手) vs Suisho5 (後手) ---", flush=True)
    morse = UsiEngine("Morse", MORSE_EXE, MORSE_DIR)
    suisho = UsiEngine("Suisho5", SUISHO_EXE, SUISHO_DIR)
    try:
        w1, m1, r1 = run_game("Morse", morse, "Suisho5", suisho, max_plies=350, byoyomi_ms=byoyomi)
    finally:
        morse.close()
        suisho.close()

    # Game 2: 先手 水匠5 vs 後手 Morse
    print("\n--- Starting Game 2: Suisho5 (先手) vs Morse (後手) ---", flush=True)
    morse = UsiEngine("Morse", MORSE_EXE, MORSE_DIR)
    suisho = UsiEngine("Suisho5", SUISHO_EXE, SUISHO_DIR)
    try:
        w2, m2, r2 = run_game("Suisho5", suisho, "Morse", morse, max_plies=350, byoyomi_ms=byoyomi)
    finally:
        morse.close()
        suisho.close()
