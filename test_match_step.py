import subprocess
from match_runner import UsiEngine, MORSE_EXE, MORSE_DIR, SUISHO_EXE, SUISHO_DIR

def test():
    morse = UsiEngine("Morse", MORSE_EXE, MORSE_DIR)
    suisho = UsiEngine("Suisho5", SUISHO_EXE, SUISHO_DIR)
    try:
        moves = []
        # Ply 1: Morse
        m1, s1, pv1, b1 = morse.go(moves, byoyomi_ms=500)
        print("Ply 1 Morse:", m1)
        moves.append(m1)
        
        # Ply 2: Suisho5
        m2, s2, pv2, b2 = suisho.go(moves, byoyomi_ms=500)
        print("Ply 2 Suisho5:", m2, "eval:", s2)
        print("  PV:", pv2)
        moves.append(m2)
    finally:
        morse.close()
        suisho.close()

if __name__ == "__main__":
    test()
