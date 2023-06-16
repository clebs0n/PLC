-- Basic Types

-- ou exclusivo (xor)
xor :: Bool -> Bool -> Bool
xor in_1 in_2 
    | in_1 == False && in_2 == False   = False
    | otherwise                = not(in_1 && in_2)

xor_int :: Int -> Int -> Bool
xor_int a b 
    | (a == 0 && b == 0) || (a == 1 && b == 1)     = False
    | otherwise           = True


-- Characters
-- Código pra rodar o exemplo com \t e \n abaixo
-- putStrLn "This is an example\twith\ttabs\nand\nnewlines."

-- Int para Char
number :: Int -> Char
number a = toEnum(a)

-- Char para Int
char_int :: Char -> Int
char_int a = fromEnum(a)

-- Minúscula pra maiúscula
maiuscula :: Char -> Char
maiuscula x = number(char_int(x) - (char_int('a') - char_int('A')))

-- Se é número ou não
ehDigito :: Char -> Bool
ehDigito x = char_int(x) >= 48 && char_int(x) <= 57 

-- Função que adiciona espaços
addEspacos :: Int -> String
addEspacos x
    | x == 0      = ""
    | otherwise   = ("o" ++ addEspacos(x-1))

-- Função que coloca espaços em branco em uma frase/palavra
paraDireita :: Int -> String -> String
paraDireita x pal = addEspacos(x) ++ pal


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
    | x == -1    = 0
    | otherwise = vendas x + totalVendas(x - 1)

-- Vendas nulas (slide 3)
vendaNula :: Int -> Bool
vendaNula x = (vendas x) == 0

-- 

cabecalho :: String
cabecalho = "Semana - Venda\n"

imprimeSemanas :: Int -> String
imprimeSemanas n 
    | n == -1      = ""
    | otherwise   = imprimeSemanas(n - 1) ++ "   " ++ show(n) ++ "  -  " ++ show(vendas n) ++ "\n"

imprimeTotal :: Int -> String
imprimeTotal n = "Total    " ++ show(totalVendas(n)) ++ "\n"

imprimeMedia :: Int -> String
imprimeMedia n = "Media    " ++ show(totalVendas(n) `div`  (n+1)) ++ "\n"
--

-- Imprime tabela com as informações de venda
imprimeTabela :: Int -> IO()
imprimeTabela n = putStr(cabecalho 
                  ++ imprimeSemanas n
                  ++ imprimeTotal n
                  ++ imprimeMedia n)


-- Soma elementos de uma tupla de inteiros
addPair :: (Int, Int) -> Int
addPair (x,y) = x + y

-- Faz shift em uma tupla de tupla de inteiro e inteiro
shiftt :: ((Int, Int), Int) -> (Int, (Int, Int))
shiftt ((x,y),z) = (x,(y,z))

-- exercicio menor que
maxi :: Int -> Int -> Int
maxi x y | x >= y    = x
         | otherwise = y

menor :: Int -> Int -> Int
menor x y | x <= y    = x
         | otherwise = y

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z = (menor (menor x y) z, maxi (maxi x y) z)






