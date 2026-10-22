import sys

# 1. Update ShogiUsiMorse-v4.hs
with open('ShogiUsiMorse-v4.hs', 'r', encoding='utf-8') as f:
    shogi_code = f.read()

target_str = """            rookRetreatBonus = case m of
              NormalMove from to _ ->
                case Map.lookup from curBd of
                  Just (Piece p Rook) | p == plr ->
                    let isUnderAttack = isSquareUnderAttack opp from curBd
                        isRetreatToHome = if plr == Sente then to == (2, 8) else to == (8, 2)
                    in if isUnderAttack && isRetreatToHome then 1200.0
                       else if isRetreatToHome then 600.0
                       else 0.0
                  _ -> 0.0
              _ -> 0.0
        in if hasOppBishopInHand
           then kobinScore + rookRetreatBonus
           else rookRetreatBonus
          
  in pawnDropDefenseScore + solidDefenseScore + antiForkDefenseScore + antiBishopDropAndKobinDefenseScore"""

replacement_str = """            rookRetreatBonus = case m of
              NormalMove from to _ ->
                case Map.lookup from curBd of
                  Just (Piece p Rook) | p == plr ->
                    let isUnderAttack = isSquareUnderAttack opp from curBd
                        isRetreatToHome = if plr == Sente then to == (2, 8) else to == (8, 2)
                    in if isUnderAttack && isRetreatToHome then 1200.0
                       else if isRetreatToHome then 600.0
                       else 0.0
                  _ -> 0.0
              _ -> 0.0
        in if hasOppBishopInHand
           then kobinScore + rookRetreatBonus
           else rookRetreatBonus
      
      -- 5. ★ 4枚の攻めを受けきる守り（金、銀、桂、香の連携受け・底香・底歩）
      fourPieceDefenseCoverScore =
        let toSq = case m of NormalMove _ t _ -> t; DropMove _ t -> t
            ptMoved = case m of
              NormalMove f _ _ -> case Map.lookup f curBd of Just (Piece _ pt) -> pt; Nothing -> Pawn
              DropMove pt _    -> pt
            isInMyCamp = if plr == Sente then snd toSq >= 7 else snd toSq <= 3
            isGuardPiece = ptMoved `elem` [Gold, Silver, Knight, Lance, Pawn]
            isNearKing = case nextKingSq of
              Just (kx, ky) -> abs (fst toSq - kx) <= 3 && abs (snd toSq - ky) <= 3
              Nothing       -> False
            isCoveringAttackedPiece = any (\\(sq, Piece own _) -> own == plr && isSquareUnderAttack opp sq curBd && isSquareUnderAttack plr sq nextBd) (Map.toList nextBd)
            oppAttackersOnTo = getPiecesCovering opp toSq nextBd
            myDefendersOnTo = getPiecesCovering plr toSq nextBd
            isInterposingOrSolidifying = not (null oppAttackersOnTo) && not (null myDefendersOnTo)
        in if isGuardPiece && (isInMyCamp || isNearKing)
           then if isInterposingOrSolidifying then 450.0
                else if isCoveringAttackedPiece then 350.0
                else if ptMoved == Lance && (snd toSq == 9 || snd toSq == 1) then 300.0
                else 100.0
           else 0.0
          
  in pawnDropDefenseScore + solidDefenseScore + antiForkDefenseScore + antiBishopDropAndKobinDefenseScore + fourPieceDefenseCoverScore"""

if target_str in shogi_code:
    shogi_code = shogi_code.replace(target_str, replacement_str, 1)
    with open('ShogiUsiMorse-v4.hs', 'w', encoding='utf-8') as f:
        f.write(shogi_code)
    print("Updated ShogiUsiMorse-v4.hs with fourPieceDefenseCoverScore successfully!")
else:
    print("Could not find target_str in ShogiUsiMorse-v4.hs!")

# 2. Update GenerateStratifiedCompleteBook.hs
with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    book_code = f.read()

target_book_str = """      -- 3. ★ 4枚の攻め（銀・桂の前進・援護）への積極加点
      isFourPieceAdvance = ptMoved `elem` [Silver, Knight] && (if plr == Sente then snd toSq <= 5 else snd toSq >= 5)
      fourPieceBonus = if isFourPieceAdvance then 350.0 else 0.0
  in capVal + promoVal + checkVal + suicidePen + fourPieceBonus"""

replacement_book_str = """      -- 3. ★ 4枚の攻め（銀・桂の前進・援護）への積極加点
      isFourPieceAdvance = ptMoved `elem` [Silver, Knight] && (if plr == Sente then snd toSq <= 5 else snd toSq >= 5)
      fourPieceBonus = if isFourPieceAdvance then 350.0 else 0.0
      
      -- 4. ★ 4枚の攻めを受けきる守り（金・銀・桂・香）の確立と受けきり加点
      myKingSq = findKing plr nextBd
      isDefendingMyKing = case myKingSq of
        Just (mkx, mky) -> abs (fst toSq - mkx) <= 3 && abs (snd toSq - mky) <= 3 && ptMoved `elem` [Gold, Silver, Knight, Lance, Pawn]
        Nothing         -> False
      fourPieceDefenseBonus =
        if isDefendingMyKing && not (null oppAttackers) && not (null friendlyDefenders)
        then 800.0 -- 相手の攻めを金銀桂香でしっかり受け止めた！
        else if isDefendingMyKing then 250.0 else 0.0
        
      -- 5. ★ 1手勝ち（速度勝ち: Speed Race Advantage）判定
      -- 自玉が安全（王手なし・自玉近傍の圧力2以下）で、敵玉に王手または2マス以内に迫る
      isMyKingSafe = not (isKingUnderAttack plr nextBd) &&
                     (case myKingSq of
                        Just (mkx, mky) ->
                          let myNeighbors = [ (mkx + dx, mky + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0), not (isOutOfBounds (mkx + dx, mky + dy)) ]
                              oppPressures = length [ sq | sq <- myNeighbors, isCoveredBy opp sq nextBd ]
                          in oppPressures <= 2
                        Nothing -> False)
      oppKingSq = findKing opp nextBd
      isPressuringOppKing = case oppKingSq of
        Just (okx, oky) -> abs (fst toSq - okx) <= 2 && abs (snd toSq - oky) <= 2
        Nothing         -> False
      isCheck = isKingUnderAttack opp nextBd
      oneTempoWinBonus = if isMyKingSafe && (isCheck || isPressuringOppKing)
                         then 1200.0 -- 1手勝ち確定！相手の攻めを受けきって敵玉を仕留める！
                         else 0.0
  in capVal + promoVal + checkVal + suicidePen + fourPieceBonus + fourPieceDefenseBonus + oneTempoWinBonus"""

if target_book_str in book_code:
    book_code = book_code.replace(target_book_str, replacement_book_str, 1)
    with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
        f.write(book_code)
    print("Updated GenerateStratifiedCompleteBook.hs with 4-piece defense and 1-tempo win successfully!")
else:
    print("Could not find target_book_str in GenerateStratifiedCompleteBook.hs!")
