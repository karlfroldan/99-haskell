notDivisible :: Int -> Int -> Bool 
a `notDivisible` b = a `mod` b /= 0

sieve :: Int -> [Int]
sieve n = sieve' 2 [2..n]
          where 
              sieve' :: Int -> [Int] -> [Int]
              sieve' n nx = sieve' 
              sieve' i nx = i : sieve' i [m | m <- tail nx, m `notDivisible` i]