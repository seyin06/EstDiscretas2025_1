longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sumaLista :: Num a =>[a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento lista elemento valor =
  if valor
  then elemento : lista
  else lista ++ [elemento]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "La lista está vacía"
maximoLista [x] = x
maximoLista (x:xs) =
  if x > maximoLista xs
  then x
  else maximoLista xs

indice :: [a] -> Int -> a
indice [] = error "La lista está vacía"                              
indice (_:xs) n =
    if n > 0 && n <= longitud xs
    then indice xs (n-1)
    else if n == 0
    then error "El número no es válido"
    else error "Indice fuera de rango"

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n],mod n x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [p|p <- xs, p /= x]

numerosPares :: [Int] -> [Int]
numerosPares (x:xs)  = [p|p <- (x:xs), p `mod` 2 == 0]
