import System.Random ( mkStdGen, RandomGen(genWord16), StdGen )

tailGuard :: [a] -> [a]
tailGuard [] = []
tailGuard xs = tail xs

removedItem :: Int -> [a] -> [a]
removedItem n xs = let (pre, suc) = splitAt n xs
                  in  pre ++ tailGuard suc

rndSelect :: [a] -> Int -> [a]
rndSelect xs n  = rndSelect' xs n (mkStdGen 128)


rndSelect' :: [a] -> Int -> StdGen -> [a]
rndSelect' _ 0 _ = []
rndSelect' [] _ _ = []
rndSelect' xs n g = (xs !! r) : rndSelect' (removedItem r xs) (n - 1) h 
    where (rGen, h) = genWord16 g 
          r         = fromIntegral rGen `mod` length xs