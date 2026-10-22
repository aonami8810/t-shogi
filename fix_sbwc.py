with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    lines = f.readlines()

new_sbwc = [
    '                  let (bestScore, pv) = searchAlphaBeta weights 5 (-99999.0) 99999.0 state\n',
    '                  in case pv of\n',
    '                       (bestMove:_) | bestMove `elem` f5FilteredMoves ->\n',
    '                         let newCache = Map.insert (positionKey state) (bestScore, bestMove) (cache state)\n',
    '                         in (Right bestMove, state { cache = newCache })\n',
    '                       _ ->\n',
    '                         case effectiveMoves of\n',
    '                           (m:_) -> (Right m, state)\n',
    '                           []    -> (Left "resign", state)\n'
]

lines[2962:2971] = new_sbwc

with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
    f.writelines(lines)

print('Updated searchBestMoveWithCache successfully.')
