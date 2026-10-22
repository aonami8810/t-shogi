with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    code = f.read()

target = """                    -- 3. 飛び駒を敵玉と同じライン（筋・段・斜め）に据える手、または桂馬の急襲
                    potentialAlignBonus = case m of
                      NormalMove _ to _ ->
                        case findKing opp (board st) of
                          Just (ekx, eky) ->
                            let (tx, ty) = to
                                isAligned = tx == ekx || ty == eky || abs (tx - ekx) == abs (ty - eky)
                            in if isAligned then 180.0 else 0.0
                          Nothing -> 0.0"""

replacement = """                    -- 3. 飛び駒を敵玉と同じライン（筋・段・斜め）に据える手、または桂馬の急襲
                    potentialAlignBonus =
                      if ptMoved `elem` [Rook, PRook, Bishop, PBishop, Lance, Knight]
                      then case m of
                        NormalMove _ to _ ->
                          case findKing opp (board st) of
                            Just (ekx, eky) ->
                              let (tx, ty) = to
                                  isAligned = (ptMoved `elem` [Rook, PRook, Lance] && tx == ekx) ||
                                              (ptMoved `elem` [Rook, PRook] && ty == eky) ||
                                              (ptMoved `elem` [Bishop, PBishop] && abs (tx - ekx) == abs (ty - eky)) ||
                                              (ptMoved == Knight && abs (tx - ekx) <= 2 && abs (ty - eky) <= 2)
                              in if isAligned then 180.0 else 0.0
                            Nothing -> 0.0
                        _ -> 0.0
                      else 0.0"""

c_norm = code.replace('\r\n', '\n')
t_norm = target.replace('\r\n', '\n')
r_norm = replacement.replace('\r\n', '\n')

if t_norm in c_norm:
    c_norm = c_norm.replace(t_norm, r_norm, 1)
    with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
        f.write(c_norm)
    print("Successfully replaced potentialAlignBonus!")
else:
    print("Target not found in ShogiUsiMorse-v4.hs")
