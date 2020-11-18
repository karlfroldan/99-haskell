slice :: [a] -> Int -> Int -> [a]
slice xs y z = (drop (y - 1) . take z) xs