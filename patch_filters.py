with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    content = f.read()

target = """      else if senteRookBreach && hasPawn
      then case m of
             DropMove Pawn (8, 7) -> False -- 8七歩打のみ許可！
             _                    -> True  -- それ以外のすべての手抜き手は完全除外！
      else if goteRookBreach && hasPawn
      then case m of
             DropMove Pawn (2, 3) -> False -- 2三歩打のみ許可！
             _                    -> True  -- それ以外のすべての手抜き手は完全除外！
      else False"""

replacement = """      else if senteRookBreach && hasPawn
      then case m of
             DropMove Pawn (8, 7) -> False -- 8七歩打のみ許可！
             NormalMove (6, 9) (7, 8) False -> False -- 7八金も許可！
             NormalMove (7, 9) (7, 8) False -> False -- 7八銀も許可！
             _                    -> True  -- それ以外のすべての手抜き手は完全除外！
      else if goteRookBreach && hasPawn
      then case m of
             DropMove Pawn (2, 3) -> False -- 2三歩打のみ許可！
             NormalMove (4, 1) (3, 2) False -> False -- 3二金も許可！（相掛かり二枚竜完封）
             NormalMove (3, 1) (3, 2) False -> False -- 3二金も許可！
             _                    -> True  -- それ以外のすべての手抜き手は完全除外！
      else False"""

# CRLF / LF を考慮して置換
if target in content:
    content = content.replace(target, replacement)
    print("Replaced with LF match!")
elif target.replace('\n', '\r\n') in content:
    content = content.replace(target.replace('\n', '\r\n'), replacement.replace('\n', '\r\n'))
    print("Replaced with CRLF match!")
else:
    print("Target not found directly, searching substring...")
    import re
    patt = re.compile(r"else if senteRookBreach && hasPawn.*?else False", re.DOTALL)
    m = patt.search(content)
    if m:
        content = content[:m.start()] + replacement + content[m.end():]
        print("Replaced with regex!")
    else:
        print("Regex match failed!")

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(content)
