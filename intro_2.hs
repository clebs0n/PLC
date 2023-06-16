-- Notações básicas
answer :: Int
answer = 42

greater :: Bool
greater = (answer > 71)

yes :: Bool
yes = True

-- Definição de funções
square :: Int -> Int
square x = x * x

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

maxi :: Int -> Int -> Int
maxi x y | x >= y    = x
         | otherwise = y

-- Exemplo vendas

vendas 0 = 10
vendas 1 = 24
vendas 2 = 45   
vendas 3 = 74
vendas 4 = 13
vendas 5 = 85
vendas 6 = 56
vendas 7 = 56
vendas 8 = 345
vendas 9 = 3434
vendas 10 = 435345
vendas _ = 0

totalVendas :: Int -> Int
totalVendas x
    | x == 0    = 0
    | otherwise = vendas x + totalVendas(x - 1)


-- Casamento de padrões

maxVendas :: Int -> Int
maxVendas 0 = vendas 0
maxVendas n = maxi (maxVendas(n - 1))(vendas n)

-- fatorial
fat :: Int -> Int
fat x
    | x == 0      = 1
    | otherwise   = x * fat(x - 1)

-- all4Equal

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = allEqual a b c && allEqual b c d

-- Definições locais

sumSquares :: Int -> Int -> Int
sumSquares x y = sqx + sqy
    where sqx = x * x
          sqy = y * y




