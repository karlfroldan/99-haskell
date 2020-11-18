removeAt :: Int -> [a] -> (a, [a])
removeAt x ys = let pre = take (x - 1) ys
                    suf = drop x ys
                in  (ys !! (x - 1), pre ++ suf)