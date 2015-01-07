minimax :: Game g => g -> GTree g -> (Maybe (Move g), Value)
minimax g (Tree (p, s) []) = (Nothing, (value g p s))
minimax g (Tree (True, s) v) = maximumSnd [(Just m, snd $ minimax g ch) | (m,ch) <- v]
minimax g (Tree (False, s) v) = minimumSnd [(Just m, snd $ minimax g ch) | (m,ch) <- v]
