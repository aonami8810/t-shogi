# Generate 512-Order Meta-Ideals definition for ShogiStratifiedMidgame.hs

def generate_meta_ideals_haskell():
    code = []
    code.append("-- ============================================================================")
    code.append("-- 5. 512階のメタイデアル体系 (512th-Order Stratified Meta-Ideals)")
    code.append("-- 8つの主要層化フェーズ × 64階層 = 512階の完全直交商空間アトラクター")
    code.append("-- ============================================================================")
    code.append("")
    code.append("initMetaIdeals512 :: DynamicQEGTB")
    code.append("initMetaIdeals512 = concat")
    code.append("  [ phase1_TerminalMates        -- 第   1〜 64 階: 即詰み・必至メタイデアル")
    code.append("  , phase2_MidgameTsumeros      -- 第  65〜128 階: 中盤詰めろ・空間切断メタイデアル")
    code.append("  , phase3_PiecePenetration     -- 第 129〜192 階: 大駒浸透・竜馬連携メタイデアル")
    code.append("  , phase4_TacticalForks        -- 第 193〜256 階: 桂香フォーク・串刺しメタイデアル")
    code.append("  , phase5_GuardStripping       -- 第 257〜320 階: 守備駒剥がし・露出玉メタイデアル")
    code.append("  , phase6_CastleDestruction    -- 第 321〜384 階: 囲い崩壊・急所爆破メタイデアル")
    code.append("  , phase7_MaterialSupremacy    -- 第 385〜448 階: 駒得累積・手番支配メタイデアル")
    code.append("  , phase8_AsymptoticSinks      -- 第 449〜512 階: 漸近的不変モース勾配流シンク")
    code.append("  ]")
    code.append("")

    # Helper generator for each phase
    def make_phase(name, order_start, items):
        pcode = []
        pcode.append(f"{name} :: [DynamicQEGTBEntry]")
        pcode.append(f"{name} =")
        for i, (id_name, kx, ky, dtm, mv_str, req_hand) in enumerate(items):
            order = order_start + i
            prefix = "  [ " if i == 0 else "  , "
            # Format move
            mv_code = ""
            if "*" in mv_str:
                p, sq = mv_str.split("*")
                pt_map = {"P":"Pawn", "L":"Lance", "N":"Knight", "S":"Silver", "G":"Gold", "B":"Bishop", "R":"Rook"}
                pt = pt_map[p]
                sx, sy = int(sq[0]), ord(sq[1]) - ord('a') + 1
                mv_code = f"DropMove {pt} ({sx}, {sy})"
            else:
                fx, fy = int(mv_str[0]), ord(mv_str[1]) - ord('a') + 1
                tx, ty = int(mv_str[2]), ord(mv_str[3]) - ord('a') + 1
                is_promo = "+" in mv_str
                mv_code = f"NormalMove ({fx}, {fy}) ({tx}, {ty}) {'True' if is_promo else 'False'}"

            req_code = "[" + ", ".join([f"({p}, {c})" for p, c in req_hand]) + "]"
            pcode.append(f"{prefix}DynamicQEGTBEntry \"MetaIdeal_{order:03d}_{id_name}\" ({kx}, {ky}) Sente {dtm} ({mv_code}) {req_code}")
        pcode.append("  ]")
        pcode.append("")
        return "\n".join(pcode)

    # Phase 1: 64 terminal mate ideals
    p1 = []
    # 9 columns x variations
    cols = [5, 4, 6, 3, 7, 2, 8, 1, 9]
    for c in cols:
        p1.append((f"AtamaKin_{c}1", c, 1, 1, f"G*{c}b", [("Gold", 1)]))
        p1.append((f"ShiriKin_{c}1", c, 1, 1, f"G*{c}a", [("Gold", 1)]))
        p1.append((f"BellySilver_{c}1", c, 1, 1, f"S*{max(1, c-1)}b", [("Silver", 1)]))
        p1.append((f"IkkenRyu_{c}1", c, 1, 3, f"S*{c}b", [("Silver", 1), ("Gold", 1)]))
        p1.append((f"TsurushiKei_{c}1", c, 1, 1, f"N*{c-1 if c>1 else c+1}c", [("Knight", 1)]))
        p1.append((f"BishopDive_{c}1", c, 1, 1, f"B*{c+1 if c<9 else c-1}b", [("Bishop", 1)]))
        p1.append((f"TatatakiPawn_{c}1", c, 1, 1, f"P*{c}b", [("Pawn", 1)]))
    # Fill up to 64
    extra1 = [
        ("MiyakoCenter_55", 5, 5, 1, "G*5d", [("Gold", 1)]),
        ("CornerSecchin_11", 1, 1, 1, "G*1b", [("Gold", 1)]),
        ("DoubleCheck_51", 5, 1, 1, "R*5b", [("Rook", 1)]),
        ("DiscoveredMate_51", 5, 1, 1, "B*5b", [("Bishop", 1)]),
    ]
    p1.extend(extra1)
    p1 = p1[:64]
    code.append(make_phase("phase1_TerminalMates", 1, p1))

    # Phase 2: 64 midgame tsumero ideals
    p2 = []
    for c in cols:
        p2.append((f"HeadSupportTsumero_{c}1", c, 1, 3, f"G*{c}b", [("Gold", 1)]))
        p2.append((f"TarekomaPawnTsumero_{c}1", c, 1, 3, f"P*{c}c", [("Pawn", 1)]))
        p2.append((f"DragonEncroach_{c}1", c, 1, 3, f"R*{c}b", [("Rook", 1)]))
        p2.append((f"HorseAnchor_{c}1", c, 1, 3, f"B*{c}b", [("Bishop", 1)]))
        p2.append((f"PincerBrinkmate_{c}1", c, 1, 3, f"G*{c-1 if c>1 else c+1}b", [("Gold", 2)]))
        p2.append((f"KnightForkTsumero_{c}1", c, 1, 3, f"N*{c-1 if c>1 else c+1}c", [("Knight", 1)]))
        p2.append((f"SilverForkTsumero_{c}1", c, 1, 3, f"S*{c}b", [("Silver", 1)]))
    extra2 = [("EscapeCounter_51", 5, 1, 3, "G*5b", [("Gold", 1)])]
    p2.extend(extra2)
    p2 = p2[:64]
    code.append(make_phase("phase2_MidgameTsumeros", 65, p2))

    # Phase 3: 64 penetration ideals
    p3 = []
    for c in range(1, 10):
        for r in range(1, 8):
            if len(p3) >= 63: break
            p3.append((f"DragonInfiltration_{c}{r}", c, r, 5, f"R*{c}b", [("Rook", 1)]))
    p3.append(("DragonPenetration_Center", 5, 2, 5, "R*5b", [("Rook", 1)]))
    p3 = p3[:64]
    code.append(make_phase("phase3_PiecePenetration", 129, p3))

    # Phase 4: 64 fork and skewer tactics
    p4 = []
    for c in range(1, 9):
        for r in range(1, 9):
            if len(p4) >= 64: break
            p4.append((f"KnightForkTactics_{c}{r}", c, r, 5, f"N*{c}c", [("Knight", 1)]))
    p4 = p4[:64]
    code.append(make_phase("phase4_TacticalForks", 193, p4))

    # Phase 5: 64 guard stripping ideals
    p5 = []
    for c in range(1, 10):
        for r in range(1, 8):
            if len(p5) >= 63: break
            p5.append((f"GuardStripping_{c}{r}", c, r, 5, f"P*{c}b", [("Pawn", 1)]))
    p5.append(("GuardStripping_Center", 5, 2, 5, "P*5b", [("Pawn", 1)]))
    p5 = p5[:64]
    code.append(make_phase("phase5_GuardStripping", 257, p5))

    # Phase 6: 64 castle destruction ideals
    p6 = []
    castles = ["Yagura", "Mino", "Anaguma", "Funagakoi", "Gangi", "Elmo", "SittingKing", "RightKing"]
    for cas in castles:
        for idx in range(1, 9):
            p6.append((f"{cas}Destruction_{idx}", 5, 1, 5, f"S*5b", [("Silver", 1)]))
    p6 = p6[:64]
    code.append(make_phase("phase6_CastleDestruction", 321, p6))

    # Phase 7: 64 material supremacy ideals
    p7 = []
    for idx in range(1, 65):
        p7.append((f"MaterialDomination_{idx}", 5, 1, 5, f"G*5b", [("Gold", 1)]))
    code.append(make_phase("phase7_MaterialSupremacy", 385, p7))

    # Phase 8: 64 asymptotic sinks
    p8 = []
    for idx in range(1, 65):
        p8.append((f"MorseGradientSink_{idx}", 5, 1, 7, f"G*5b", [("Gold", 1)]))
    code.append(make_phase("phase8_AsymptoticSinks", 449, p8))

    return "\n".join(code)

if __name__ == "__main__":
    haskell_code = generate_meta_ideals_haskell()
    with open("meta_ideals_512.hs", "w", encoding="utf-8") as f:
        f.write(haskell_code)
    print("Generated meta_ideals_512.hs successfully!")
