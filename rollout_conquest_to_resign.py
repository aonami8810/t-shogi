# -*- coding: utf-8 -*-
import subprocess
import time
from shogi_state import BoardState

SUISHO_EXE = r"C:\Suisho5-YaneuraOu-v7.5.0-windows\YaneuraOu_NNUE-tournament-clang++-zen2.exe"
SUISHO_DIR = r"C:\Suisho5-YaneuraOu-v7.5.0-windows"

class SuishoSession:
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
        self.send("usinewgame")

    def send(self, cmd):
        self.proc.stdin.write(cmd + "\n")
        self.proc.stdin.flush()

    def read_until(self, target):
        lines = []
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            lines.append(line)
            if target in line: break
        return lines

    def query_multipv(self, moves, byoyomi_ms=1000, multipv=5):
        self.send(f"setoption name MultiPV value {multipv}")
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")

        candidates = {} # mpv_index -> (move, score_val, score_str, pv)
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            if line.startswith("info") and "multipv" in line and "score" in line and "pv" in line:
                parts = line.split()
                try:
                    mpv_idx = int(parts[parts.index("multipv") + 1])
                    pv_idx = parts.index("pv")
                    first_move = parts[pv_idx + 1]
                    
                    score_val = 0
                    score_str = ""
                    if "cp" in parts:
                        cp_idx = parts.index("cp")
                        score_val = int(parts[cp_idx + 1])
                        score_str = f"cp {score_val}"
                    elif "mate" in parts:
                        m_idx = parts.index("mate")
                        m_val = parts[m_idx + 1]
                        if m_val.startswith("+") or (not m_val.startswith("-") and int(m_val) > 0):
                            score_val = 30000 - int(m_val.lstrip("+"))
                        else:
                            score_val = -30000 - int(m_val)
                        score_str = f"mate {m_val}"
                    candidates[mpv_idx] = (first_move, score_val, score_str, line)
                except Exception:
                    pass
            if line.startswith("bestmove"):
                break
        return candidates

    def query_opponent(self, moves, byoyomi_ms=500):
        # 相手（水匠5）は通常の single PV, 500ms
        self.send("setoption name MultiPV value 1")
        if moves:
            self.send(f"position startpos moves {' '.join(moves)}")
        else:
            self.send("position startpos")
        self.send(f"go btime 0 wtime 0 byoyomi {byoyomi_ms}")

        bestmove = None
        score_str = ""
        while True:
            line = self.proc.stdout.readline()
            if not line: break
            line = line.strip()
            if line.startswith("info") and "score" in line and "pv" in line:
                parts = line.split()
                if "cp" in parts:
                    score_str = f"cp {parts[parts.index('cp') + 1]}"
                elif "mate" in parts:
                    score_str = f"mate {parts[parts.index('mate') + 1]}"
            if line.startswith("bestmove"):
                bestmove = line.split()[1]
                break
        return bestmove, score_str

    def close(self):
        try:
            self.send("quit")
            self.proc.wait(timeout=2)
        except Exception:
            self.proc.kill()

def play_conquest(name, start_moves, morse_side, max_plies=256):
    print(f"\n==================================================", flush=True)
    print(f"  Starting Conquest Rollout: {name} (Morse={morse_side})", flush=True)
    print(f"==================================================", flush=True)

    session = SuishoSession()
    moves = list(start_moves)

    # 盤面ステート初期化
    bs = BoardState()
    history_keys = {bs.get_key(): 1}
    for m in moves:
        bs.apply_move(m)
        k = bs.get_key()
        history_keys[k] = history_keys.get(k, 0) + 1

    try:
        for ply in range(len(moves) + 1, max_plies + 1):
            is_morse = (morse_side == 'Sente' and ply % 2 == 1) or (morse_side == 'Gote' and ply % 2 == 0)
            turn_str = "先手" if ply % 2 == 1 else "後手"
            role_str = "Morse" if is_morse else "Suisho5"

            if is_morse:
                # Morse側: 水匠5のMultiPVで千日手を回避しつつ最強の手を選択
                cands = session.query_multipv(moves, byoyomi_ms=1000, multipv=8)
                
                # 有効手の選別（千日手にならない候補）
                valid_cands = []
                for mpv_idx, (mv, sc_val, sc_str, pv_line) in sorted(cands.items()):
                    next_bs = bs.copy()
                    next_bs.apply_move(mv)
                    next_k = next_bs.get_key()
                    # すでに過去に1回以上出ている局面なら千日手危険度大
                    repetition_count = history_keys.get(next_k, 0)
                    valid_cands.append((repetition_count, -sc_val if morse_side == 'Gote' else sc_val, mv, sc_str))
                
                # ソート: repetition_count が小さい（0優先）、スコアが高い順
                valid_cands.sort(key=lambda x: (x[0], -x[1]))
                
                if not valid_cands:
                    # 候補なし？
                    bm = "resign"
                    sc_tag = "no valid candidate"
                else:
                    rep_cnt, _, chosen_mv, sc_tag = valid_cands[0]
                    bm = chosen_mv
                    if rep_cnt > 0:
                        sc_tag += f" [REP {rep_cnt}]"
                
                print(f"Ply {ply:3d} ({turn_str} {role_str:7s}): {bm:6s} (eval: {sc_tag})", flush=True)

            else:
                # 水匠5側: 実戦本番通り byoyomi 500ms で応手
                bm, sc_str = session.query_opponent(moves, byoyomi_ms=500)
                print(f"Ply {ply:3d} ({turn_str} {role_str:7s}): {bm:6s} (eval: {sc_str})", flush=True)

            if bm == "resign":
                winner = "Morse" if not is_morse else "Suisho5"
                print(f"\n>>> MATCH TERMINATED AT PLY {ply}! Winner: {winner} <<<", flush=True)
                moves.append(bm)
                break

            moves.append(bm)
            bs.apply_move(bm)
            k = bs.get_key()
            history_keys[k] = history_keys.get(k, 0) + 1

    finally:
        session.close()

    return moves

if __name__ == "__main__":
    # 1. Game 1: 25手目 P*2d, 26手目 R*2g に対し、27手目 7g3c+ (銀で角を取る最強打 +276点) から開始！
    g1_prefix = "7g7f 3c3d 2g2f 8c8d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d R*2g 7g3c+".split()
    res_g1 = play_conquest("Game 1 (Morse 先手)", g1_prefix, 'Sente', max_plies=250)
    print("G1_FINAL =", res_g1)

    # 2. Game 2: 7手目 5i5h に対し、8手目 4a3b (3二金！ -107点) から開始！
    g2_prefix = "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 5i5h 4a3b".split()
    res_g2 = play_conquest("Game 2 (Morse 後手)", g2_prefix, 'Gote', max_plies=250)
    print("G2_FINAL =", res_g2)
