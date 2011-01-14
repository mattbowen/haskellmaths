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
removeFst :: Int -> [Int] -> [Int]
removeFst m [] = []
removeFst m (x:xs) | m == x    = xs
                   | otherwise = x:(removeFst m xs)

-- ex 1.13 
count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | c == x    = 1 + count c xs
               | otherwise = count c xs

-- ex 1.14
-- I feel like passing l to blowup' in every call is unnecessary, but I can't think
-- of another way to figure out where I am in the list but pass in the total length
blowup :: String -> String
blowup x = blowup' (length x) x
blowup' l "" = ""
blowup' l (x:xs) = (replicate' (l - length xs) x) ++ (blowup' l xs)

-- Matt's very own replicate, to see whether I could build it out what what I already know
replicate' :: Int -> Char -> String
replicate' n x = [x] ++ (replicate'' (n - 1) [x])
replicate'' :: Int -> String -> String
replicate'' n x | n > 0     = x ++ (replicate'' (n -1) x)
                | otherwise = ""

-- ex 1.15
removeFst' :: String -> [String] -> [String] -- I don't understand why this can't be a -> [a] -> [a]
removeFst' m [] = []
removeFst' m (x:xs) | m == x    = xs
                   | otherwise = x:(removeFst' m xs)

mnmChar :: String -> Char
mnmChar "" = error "empty string"
mnmChar [x] = x
mnmChar (x:xs) = min x (mnmChar xs)

minString :: String -> String -> String
minString x y | min (head x) (head y) == (head x) = x
              | otherwise = y

mnmString :: [String] -> String
mnmString [] = error "empty list"
mnmString [x] = x
mnmString (x:xs) = minString x (mnmString xs)

srtStrings :: [String] -> [String]
srtStrings [] = []
srtStrings xs = m : (srtStrings (removeFst' m xs)) where m = mnmString xs

-- ex 1.17
prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

substring :: String -> String -> Bool
substring xs [] = False
substring xs (y:ys') = prefix xs (y:ys') || substring xs ys' 