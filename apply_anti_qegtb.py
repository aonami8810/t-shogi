with open('c:\\VS\\Workspace\\t-shogi\\ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

target = """                      NormalMove (3, 3) (3, 4) _ | turn st == Gote  -> 220.0
                      _ -> 0.0
                    
                    s = evaluateState weights (turn st) nextSt
                in s + blunderPen + lancePen + checkBonus + forkBonus + tacticalBonus
                   + discoveredBonus + rayUnblockBonus + potentialAlignBonus + quotientEquivBonus"""

replacement = """                      NormalMove (3, 3) (3, 4) _ | turn st == Gote  -> 220.0
                      _ -> 0.0
                    
                    -- ★ 自玉周りの指し手において、駒の配置空間が相手のQEGTBにない局面になる手を最優先！
                    antiQuotientSafetyBonus = evalAntiQuotientSafety st m
                    
                    s = evaluateState weights (turn st) nextSt
                in s + blunderPen + lancePen + checkBonus + forkBonus + tacticalBonus
                   + discoveredBonus + rayUnblockBonus + potentialAlignBonus + quotientEquivBonus
                   + antiQuotientSafetyBonus"""

# normalize CRLF to match whatever is in content
target_crlf = target.replace('\r\n', '\n').replace('\n', '\r\n')
replacement_crlf = replacement.replace('\r\n', '\n').replace('\n', '\r\n')

if target in content:
    content = content.replace(target, replacement, 1)
    print("Replaced with LF")
elif target_crlf in content:
    content = content.replace(target_crlf, replacement_crlf, 1)
    print("Replaced with CRLF")
else:
    print("ERROR: Target not found!")

with open('c:\\VS\\Workspace\\t-shogi\\ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)
