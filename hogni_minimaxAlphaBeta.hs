minimaxAlphaBeta :: Game g => g -> AlphaBeta -> GTree g -> (Maybe (Move g), Value)
minimaxAlphaBeta g (a,b) (Tree (p, s) []) = (Nothing, value g p s)
minimaxAlphaBeta g (a,b) (Tree (True, s) v) = goMax g (a,b) v Nothing
minimaxAlphaBeta g (a,b) (Tree (False, s) v) = goMin g (a,b) v Nothing

goMax g (a,b) ((m,ch):xs) move
  | val >= b = (Just m, val)
  | null xs = (newMove, val)
  | otherwise = goMax g (val,b) xs newMove
    where val = max a (snd (minimaxAlphaBeta g (a,b) ch))
          newMove = updateMoveMax a val (Just m) move

updateMoveMax a val (Just m) treak
  | a > val = treak
  | otherwise = Just m

goMin g (a,b) ((m,ch):xs) move
  | val <= a = (Just m, val)
  | null xs = (newMove, val)
  | otherwise = goMin g (a,val) xs newMove
    where val = min b (snd (minimaxAlphaBeta g (a,b) ch))
          newMove = updateMoveMin b val (Just m) move

updateMoveMin b val (Just m) treak
  | b > val = treak
  | otherwise = Just m
