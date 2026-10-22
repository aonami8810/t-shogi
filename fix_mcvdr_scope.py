file_path = "c:/VS/Workspace/t-shogi/ShogiUsiMorse-v4.hs"
with open(file_path, "r", encoding="utf-8") as f:
    content = f.read()

# Replace in loop where
old_loop_head = """  where
    loop [] curAlpha bestMv = (curAlpha, bestMv)
    loop ((m, idx):ms) curAlpha bestMv ="""

new_loop_head = """  where
    curInCheck = isKingUnderAttack (turn st) (board st)
    loop [] curAlpha bestMv = (curAlpha, bestMv)
    loop ((m, idx):ms) curAlpha bestMv ="""

content = content.replace(old_loop_head, new_loop_head)
content = content.replace("qegtbCheck = if inCheck then", "qegtbCheck = if curInCheck then")
content = content.replace("isPromo || inCheck", "isPromo || curInCheck")

with open(file_path, "w", encoding="utf-8", newline="\n") as f:
    f.write(content)
print("Updated curInCheck in where clause!")
