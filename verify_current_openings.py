import re
import strict_board_validator

with open('c:/VS/Workspace/t-shogi/GenerateMasterMorseBook.hs', 'r', encoding='utf-8') as f:
    text = f.read()

pattern = re.compile(r'\(\s*"([^"]+)"\s*,\s*"[^"]+"\s*,\s*[\d\.]+\s*,\s*\[(.*?)\]\s*\)', re.DOTALL)
matches = pattern.findall(text)

all_ok = True
print(f"Found {len(matches)} openings in GenerateMasterMorseBook.hs:")
for name, mvs_str in matches:
    mvs = [m.strip().strip('"') for m in mvs_str.split(',') if m.strip()]
    ok = strict_board_validator.validate_line(name, mvs)
    if ok:
        print(f"  [OK] {name} ({len(mvs)} moves)")
    else:
        all_ok = False
        print(f"  [FAIL] {name}")

if all_ok and len(matches) == 16:
    print("\nALL 16 OPENINGS ARE 100% PHYSICALLY LEGAL!")
