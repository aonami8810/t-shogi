with open('ShogiStratifiedMidgame.hs', 'r', encoding='utf-8') as f:
    orig = f.read()

with open('meta_ideals_512.hs', 'r', encoding='utf-8') as f:
    meta_code = f.read()

# 1. Update export list: add initMetaIdeals512
if 'initMetaIdeals512' not in orig:
    orig = orig.replace('  , initSeedQEGTB', '  , initSeedQEGTB\n  , initMetaIdeals512')

# 2. Replace initSeedQEGTB definition with initMetaIdeals512 + initSeedQEGTB = initMetaIdeals512
target_start = "initSeedQEGTB :: DynamicQEGTB\ninitSeedQEGTB ="
target_end = "isCheckmatePosition :: State -> Bool"

idx_start = orig.find(target_start)
idx_end = orig.find(target_end)

if idx_start != -1 and idx_end != -1:
    new_block = meta_code + "\n\ninitSeedQEGTB :: DynamicQEGTB\ninitSeedQEGTB = initMetaIdeals512\n\n"
    new_orig = orig[:idx_start] + new_block + orig[idx_end:]
    with open('ShogiStratifiedMidgame.hs', 'w', encoding='utf-8') as f:
        f.write(new_orig)
    print("Successfully updated ShogiStratifiedMidgame.hs with 512-Order Meta-Ideals!")
else:
    print(f"Could not find target boundaries: start={idx_start}, end={idx_end}")
