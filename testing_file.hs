double :: [Int] -> [Int]
double [] = []
double (a:as) = a * 2 : double as

member :: [Int] -> Int -> Bool
member [] _ = False
member (a:as) b | b == a = True
                | otherwise = member as b

ehdigito :: Char -> Bool
ehdigito ch = ('0' <= ch) && (ch <= '9')

digits :: String -> String
digits [] = []
digits (c:cs) | ehdigito c = c : digits cs
              | otherwise  = digits cs


sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs (a:as) = fst a + snd a : sumPairs as
sumPairs [(a,b):pairs] = a + b : sumPairs pairs