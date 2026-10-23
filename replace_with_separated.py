# -*- coding: utf-8 -*-

with open("conquest_code_separated.txt", "r", encoding="utf-8") as f:
    new_code = f.read().strip()

with open("ShogiUsiMorse-v4.hs", "r", encoding="utf-8") as f:
    content = f.read()

markers = [
    "-- | 水匠5完全撃破スーパーハイウェイ（手番別完全分離設計 ＆ 手順前後完全吸収）",
    "-- | 水匠5完全撃破スーパーハイウェイ（手番別完全分離設計）",
    "-- | 水匠5完全撃破スーパーハイウェイ"
]

start_idx = -1
for m in markers:
    idx = content.find(m)
    if idx != -1:
        start_idx = idx
        break

end_marker = "lookupBookMove :: [String] -> State -> Maybe Move"
end_idx = content.find(end_marker)

assert start_idx != -1 and end_idx != -1, f"Markers not found! start_idx={start_idx}, end_idx={end_idx}"

new_content = content[:start_idx] + new_code + "\n\n" + content[end_idx:]

with open("ShogiUsiMorse-v4.hs", "w", encoding="utf-8") as f:
    f.write(new_content)

print("Successfully replaced with updated separated highways in ShogiUsiMorse-v4.hs!")
