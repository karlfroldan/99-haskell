dupli :: [a] -> [a]
dupli [x] = [x, x]
dupli (x:xs) = dupli [x] ++ dupli xs