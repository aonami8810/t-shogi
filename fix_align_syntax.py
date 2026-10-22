with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    text = f.read()

target = """                    -- 3. 飛び駒を敵玉と同じライン（筋・段・斜め）に据える手、または桂馬の急襲
                    potentialAlignBonus =
                       if ptMoved elem [Rook, PRook, Bishop, PBishop, Lance, Knight]
                       then case m of
                         NormalMove _ to _ ->
                           case findKing opp (board st) of
                             Just (ekx, eky) ->
                               let (tx, ty) = to
                                   isAligned = (ptMoved elem [Rook, PRook, Lance] && tx == ekx) ||
                                               (ptMoved elem [Rook, PRook] && ty == eky) ||
                                               (ptMoved elem [Bishop, PBishop] && abs (tx - ekx) == abs (ty - eky)) ||
                                               (ptMoved == Knight && abs (tx - ekx) <= 2 && abs (ty - eky) <= 2)
                               in if isAligned then 180.0 else 0.0
                             Nothing -> 0.0
                         _ -> 0.0
                       else 0.0
                      DropMove pt to ->
                        case findKing opp (board st) of
                          Just (ekx, eky) ->
                            let (tx, ty) = to
                                isAligned = (pt `elem` [Rook, Lance] && tx == ekx) ||
                                            (pt == Rook && ty == eky) ||
                                            (pt == Bishop && abs (tx - ekx) == abs (ty - eky)) ||
                                            (pt == Knight && abs (tx - ekx) <= 2 && abs (ty - eky) <= 2)
                            in if isAligned then 220.0 else 0.0
                          Nothing -> 0.0"""

replacement = """                    -- 3. 飛び駒を敵玉と同じライン（筋・段・斜め）に据える手、または桂馬の急襲
                    potentialAlignBonus = case m of
                      NormalMove _ to _ ->
                        if ptMoved `elem` [Rook, PRook, Bishop, PBishop, Lance, Knight]
                        then case findKing opp (board st) of
                               Just (ekx, eky) ->
                                 let (tx, ty) = to
                                     isAligned = (ptMoved `elem` [Rook, PRook, Lance] && tx == ekx) ||
                                                 (ptMoved `elem` [Rook, PRook] && ty == eky) ||
                                                 (ptMoved `elem` [Bishop, PBishop] && abs (tx - ekx) == abs (ty - eky)) ||
                                                 (ptMoved == Knight && abs (tx - ekx) <= 2 && abs (ty - eky) <= 2)
                                 in if isAligned then 180.0 else 0.0
                               Nothing -> 0.0
                        else 0.0
                      DropMove pt to ->
                        case findKing opp (board st) of
                          Just (ekx, eky) ->
                            let (tx, ty) = to
                                isAligned = (pt `elem` [Rook, Lance] && tx == ekx) ||
                                            (pt == Rook && ty == eky) ||
                                            (pt == Bishop && abs (tx - ekx) == abs (ty - eky)) ||
                                            (pt == Knight && abs (tx - ekx) <= 2 && abs (ty - eky) <= 2)
                            in if isAligned then 220.0 else 0.0
                          Nothing -> 0.0"""

t_norm = target.replace('\r\n', '\n')
r_norm = replacement.replace('\r\n', '\n')
c_norm = text.replace('\r\n', '\n')

assert t_norm in c_norm, 'Target block not found!'
c_norm = c_norm.replace(t_norm, r_norm, 1)

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.write(c_norm)

print("Successfully fixed potentialAlignBonus structure in ShogiUsiMorse-v4.hs!")
