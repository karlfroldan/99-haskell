split :: [a] -> Int -> ([a], [a])
split xs y = (take y xs, drop y xs)