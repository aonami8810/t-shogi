import subprocess

def test_all():
    with open('GenerateMasterMorseBook.hs', 'r', encoding='utf-8') as f:
        content = f.read()

    # extract masterBookMoveSequences
    import re
    # lines with ( "name", "type", pot, [ ... ] )
    pattern = r'\(\s*"([^"]+)",\s*"([^"]+)",\s*([\d\.]+)\s*,\s*\[(.*?)\]\s*\)'
    matches = re.findall(pattern, content, re.DOTALL)
    
    print(f"Found {len(matches)} opening lines.")
    for name, ntype, pot, mvs_str in matches:
        mvs = [m.strip().strip('"') for m in mvs_str.split(',') if m.strip()]
        # check each move
        clean_mvs = [m.lstrip('+') for m in mvs]
        print(f"\n=== {name} ({len(clean_mvs)} moves) ===")
        print(f"Last moves: {clean_mvs[-6:]}")

test_all()
