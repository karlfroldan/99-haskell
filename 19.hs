rotate :: [a] -> Int -> [a]
rotate xs y = take (length xs) . drop (y `mod` length xs) $ cycle xs