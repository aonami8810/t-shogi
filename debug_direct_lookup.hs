-- -*- coding: utf-8 -*-
import ShogiUsiMorse

main :: IO ()
main = do
  let moves = words "2g2f 8c8d 7g7f 3c3d 2f2e 8d8e 6i7h 8e8f 8g8f 8b8f 2e2d 2c2d 2h2d 4a3b 2d3d 2b3c 5i5h 5a5b 3g3f 8f7f 8h7g 7f7d 3d7d 7c7d P*2d R*2f 3i3h P*2h 7g3c+ 2a3c B*3g 2f2d 3g9a+ 2h2i+ 9a8a 2i2h"
  let st = replayMoves moves startState
  putStrLn $ "Turn: " ++ show (turn st)
  putStrLn $ "Legal moves: " ++ show (map showMove (f5SyzygyFilter st (generateMoves st)))
  case lookupDirectConquestMove moves st of
    Just m -> putStrLn $ "Direct Conquest Move: " ++ showMove m
    Nothing -> putStrLn "Direct Conquest Move: NOTHING!"
