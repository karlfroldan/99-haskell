data Occurence a = Single a | Multiple Int a deriving (Show)

encodeDirect :: (Eq a) => [a] -> [Occurence a]
encodeDirect = transform . foldr run []
    where run x []      = [(1, x)]
          run x (a:acc) = if snd a == x
                      then (fst a + 1, snd a) : acc
                      else (1, x) : a : acc
          transform ys = map (\(f, s) -> if f == 1
                                         then Single s
                                         else Multiple f s) ys