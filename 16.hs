dropEvery :: [a] -> Int -> [a]
dropEvery xs y = fst $ foldl (\acc x -> if snd acc == 1
                                        then (fst acc, y)
                                        else (fst acc ++ [x], snd acc - 1)) ([], y) xs