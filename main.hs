fat :: Integer -> Integer
fat n
    | n == 0      = 1
    | otherwise   = fat(n - 1)*n

addEspacos  :: Int -> String
addEspacos n
    | n == 0      = ""
    | otherwise   = " " ++ addEspacos(n-1)

vendas :: Int -> Int
vendas 0 = 50
vendas 1 = 100
vendas 2 = 150
vendas 3 = 170
vendas 4 =120

imprimeTabela :: Int -> IO()

imprimeTabela n = putStr(cabecalho
                ++ imprimeSemanas n
                ++ imprimeTotal n
                ++ imprimeMedia n)

cabecalho = "Semana       Venda\n"

imprimeTotal :: Int -> String
imprimeTotal n = "Total    " ++ show(calcula_total n) ++ "\n"
calcula_total :: Int -> Int
calcula_total n = vendas 1

imprimeMedia:: Int -> String
imprimeMedia n = "Total    " ++ show(calcula_total n) ++ "\n"


