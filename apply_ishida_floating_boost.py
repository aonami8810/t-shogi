with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

# Replace floatingBonus in evalSilverAdvanceAndIshidaFormation
content = content.replace(
    'floatingBonus = if hasSente76R && hasSente75P then 2600.0 else 0.0',
    'floatingBonus = if hasSente76R && hasSente75P then 3400.0 else 0.0'
)
content = content.replace(
    'floatingBonus = if hasGote34R && hasGote35P then 2600.0 else 0.0',
    'floatingBonus = if hasGote34R && hasGote35P then 3400.0 else 0.0'
)

# Replace isSenteFloatingRook bonus in evalSilverAdvanceAndIshidaFlow
content = content.replace(
    'then 2600.0 -- ★ 石田流浮き飛車（最強の空中戦要塞！）',
    'then 3400.0 -- ★ 石田流浮き飛車（最強の空中戦要塞！）'
)

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)

print("Applied floating rook boost successfully!")
