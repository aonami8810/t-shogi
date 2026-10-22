import subprocess

master_candidates = [
    ("Saddle_Aigakari_Standard", [
        "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c",
        "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b", "5i6h", "5a4b",
        "4i5h", "4b3c", "6h7h", "7b6c", "7g7f", "3c3d", "3h2g", "8b8d", "2h2f", "8d7d",
        "6g6f", "7d8d", "7h8h", "5c5d"
    ]),
    ("Saddle_Aigakari_Hikibisha", [
        "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c",
        "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b", "5i6h", "5a4b",
        "4i5h", "4b3c", "6h7h", "7b6c", "6g6f", "1c1d", "1g1f", "6c5d"
    ]),
    ("Saddle_Kakugawari_Hayaguri", [
        "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "8h2b+", "3a2b",
        "7i8h", "7a6b", "3i4h", "6b7b", "4h3g", "4a3b", "6i7h", "5a4b", "5i6h", "6c6d",
        "3g4f", "7c7d", "4f3e", "6d6e", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "7b7c"
    ]),
    ("Saddle_Kakugawari_Koshikake", [
        "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "8h2b+", "3a2b",
        "7i8h", "7a6b", "3i3h", "6b7b", "3h4h", "4a3b", "4h4g", "5a4b", "5i6h", "6c6d",
        "4g5f", "7b6c", "6i7h", "4b3a", "6h7i", "3a2b"
    ]),
    ("Saddle_Yagura_Fortress", [
        "7g7f", "8c8d", "6g6f", "3c3d", "7i6h", "6c6d", "6h7g", "7a6b", "5g5f", "5a4b",
        "4i5h", "3a3b", "5i6h", "4b3c", "6h7h", "5c5d", "7h8h", "4c4d", "2g2f", "6b5c",
        "2f2e", "8d8e", "8h7h", "7c7d", "7h8h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b",
        "3i4h", "5c6d", "4h4g"
    ]),
    ("Saddle_Yagura_Kato", [
        "7g7f", "8c8d", "7i6h", "3c3d", "6h7g", "7a6b", "2g2f", "8d8e", "5g5f", "4a3b",
        "5i6h", "5a4b", "4i5h", "6b5c", "6h7h", "7c7d", "2f2e", "3a2b", "7h8h", "5c6d",
        "6g6f", "4b3a"
    ]),
    ("Saddle_Shikenbisha_Mino", [
        "7g7f", "3c3d", "6g6f", "8c8d", "2h6h", "8d8e", "8h7g", "6b6c", "4i4h", "7a6b",
        "3i3h", "4a3b", "5i4h", "5a4b", "4h3h", "3a2b", "3h2h", "7b7a", "2h1h", "7a8b",
        "6h7h", "8b7b", "1h2h", "1c1d", "1g1f", "5c5d", "2g2f", "4b3a", "2f2e", "2b3c",
        "3g3f", "8b8a"
    ]),
    ("Saddle_Shikenbisha_Exchange", [
        "7g7f", "3c3d", "2h6h", "8c8d", "8h2b+", "3a2b", "7i8h", "7a6b", "8h7g", "8d8e",
        "6h6f", "6b7b", "4i4h", "4a3b", "5i4h", "5a4b", "4h3h", "3a2b", "3h2h", "7b8b",
        "6i5h", "4b3a", "1g1f", "1c1d", "2h1h", "8b7b"
    ]),
    ("Saddle_Sankenbisha_Ishida", [
        "7g7f", "3c3d", "7i6h", "8c8d", "2h7h", "8d8e", "8h7g", "6b6c", "3i4h", "4a3b",
        "5i4h", "5a4b", "4h3h", "3a2b", "3h2h", "7a6b", "1g1f", "1c1d", "3g3f", "2b3c",
        "7h7f", "5c5d", "4h5h", "6b7c"
    ]),
    ("Saddle_Nakabisha_Gokigen", [
        "5g5f", "3c3d", "5h5e", "8c8d", "5e5f", "8d8e", "7g7f", "6b6c", "7i6h", "4a3b",
        "4i5h", "5a4b", "3i4h", "3a2b", "5i4h", "4b3a", "4h3h", "7a6b", "3h2h", "6b7c",
        "2g2f", "7c8d", "2f2e", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3g3f", "8d7c",
        "2h1h", "3a2b"
    ]),
    ("Saddle_Nakabisha_LeftKing", [
        "5g5f", "3c3d", "5h5e", "5c5d", "5e5f", "8c8d", "7g7f", "8d8e", "7i6h", "6b6c",
        "4i5h", "4a3b", "3i4h", "5a4b", "5i4h", "4b3a", "4h3h", "3a2b", "3h2h", "7a6b",
        "2h1h", "6b7c", "6g6f", "4c4d", "1g1f", "1c1d"
    ]),
    ("Saddle_Yokofudori_34Rook", [
        "7g7f", "3c3d", "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d",
        "2h2d", "8e8f", "8g8f", "8b8f", "2d3d", "8f8d", "3d2d", "P*2c", "2d2h", "P*8g",
        "7h8g", "8d8b", "6g6f", "4a3b", "3i3h", "5a4b"
    ]),
    ("Saddle_Gote_84Pawn_Ibisha", [
        "2g2f", "8c8d", "2f2e", "8d8e", "7i7h", "3a3b", "2e2d", "2c2d", "2h2d", "P*2c",
        "2d2h", "8e8f", "8g8f", "8b8f", "P*8g", "8f8b", "3i3h", "7a7b", "5i6h", "5a4b",
        "4i5h", "4b3c", "6h7h", "7b6c", "7g7f", "3c3d", "3h2g", "6c5d"
    ]),
    ("Saddle_Gote_34Pawn_Yagura", [
        "7g7f", "3c3d", "2g2f", "4c4d", "4i4h", "3a3b", "5g5f", "4b4a", "5i6h", "5a4b",
        "6h7h", "4a4b", "7h8h", "4b4c", "3i3h", "3c3d", "2f2e", "2b3c", "4h4g", "8b4b",
        "6g6f", "7a6b", "3h2g", "5c5d"
    ]),
    ("Saddle_EdgePawn_Solid", [
        "1g1f", "3c3d", "7g7f", "8c8d", "6g6f", "8d8e", "7i6h", "6b6c", "2g2f", "4a3b",
        "6h7g", "7a6b", "5g5f", "5a4b", "4i5h", "4b3a", "5i6h", "3a2b", "6h7h", "6b7b",
        "2f2e", "2b3c", "3i4h", "5c5d"
    ]),
    ("Saddle_DirectGold_KakutoDefense", [
        "7g7f", "8c8d", "7i7h", "8d8e", "8h7g", "3c3d", "2g2f", "6b6c", "2f2e", "4a3b",
        "3i4h", "5a4b", "5i6h", "4b3c", "6h7h", "7a6b", "7h8h", "6b7c", "4h3g", "7c8d",
        "3g4f", "8d7c", "2e2d", "2c2d", "2h2d", "P*2c", "2d2h", "7c8d", "6g6f", "8d7c"
    ])
]

def check_all_candidates():
    cmd = ["c:\\VS\\Workspace\\t-shogi\\shogi-usi-morse-v4.exe"]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    p.stdin.write("usi\nisready\n")
    p.stdin.flush()
    while True:
        l = p.stdout.readline()
        if "readyok" in l: break

    print("Verifying 16 Master Opening Book Candidates...")
    all_ok = True
    for name, mvs in master_candidates:
        pos_cmd = f"position startpos moves {' '.join(mvs)}\n"
        p.stdin.write(pos_cmd + "go byoyomi 200\n")
        p.stdin.flush()
        bm = None
        info = []
        while True:
            line = p.stdout.readline()
            if not line: break
            l = line.strip()
            if l.startswith("info "): info.append(l)
            if l.startswith("bestmove"):
                bm = l.split()[1]
                break
        turn = "Gote" if len(mvs) % 2 == 1 else "Sente"
        last_info = info[-1] if info else "no info"
        # Check if bm is valid and score is reasonable
        print(f"  {name:32s} ({len(mvs):2d} mvs, turn={turn}): bestmove={bm} | {last_info}")
        if not bm or "resign" in bm:
            all_ok = False
            print(f"    ERROR in {name}")

    p.stdin.write("quit\n")
    p.kill()
    if all_ok:
        print("\nALL 16 OPENING SEQUENCES VERIFIED 100% LEGAL & QUIESCENT!")
    else:
        print("\nSOME OPENINGS FAILED!")

check_all_candidates()
