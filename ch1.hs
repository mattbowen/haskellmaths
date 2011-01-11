-- ex 1.4
-- No, because k^2 == n implies that divides k n == True

-- ex 1.6
-- rem:: Integer -> Integer -> Integer

-- ex 1.9
maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

-- ex 1.10
removeFst :: [Int] -> Int
removeFst [] = error "empty list"
removeFst [x] = x
removeFst (x:xs) = x

-- ex 1.13 
count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | c == x    = 1 + count c xs
               | otherwise = count c xs

-- ex 1.14
blowup :: String -> String
blowup x = blowup' (length x) x
blowup' l "" = ""
blowup' l (x:xs) = replicate' (l - length xs) x ++ blowup' l xs

-- Matt's very own replicate, to see whether I could build it out what what I already know
replicate' :: Int -> Char -> String
replicate' n x = [x] ++ (replicate'' (n - 1) [x])
replicate'' :: Int -> String -> String
replicate'' n x | n > 0     = x ++ (replicate'' (n -1) x)
                | otherwise = ""

-- ex 1.15
srtString :: String -> String
srtString "" = ""
srtString (x:xs) | x <= (head xs) = [x] ++ (srtString xs)
                 | otherwise      = (srtString xs) ++ [x]