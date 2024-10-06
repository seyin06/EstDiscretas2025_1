data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0
longitud (Node _ lista)= 1 + longitud lista

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void _ = False
estaContenido (Node x xs) elemento =
      if elemento == x
      then True
      else estaContenido xs elemento

convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node a lista) = a : convertirALista lista 

conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node x lista) =
      if estaContenido lista x
      then conjunto lista
      else Node x (conjunto lista)

eliminarIndice :: List a -> Int -> List a
eliminarIndice Void indice = error "Indice fuera del rango permitido."
eliminarIndice (Node a lista) indice =
      if indice ==  0
      then lista
      else Node a (eliminarIndice lista (indice -1))

insertarIndice :: List a -> Int -> a -> List a
insertarIndice lista indice nuevoElem =
    if indice < 0 || indice > longitud lista
    then error "Indice fuera de rango permitido"
    else if indice == 0
         then Node nuevoElem lista
         else case lista of
              Void -> error "Indice fuera del rango permitido."
              Node a resto -> Node a (insertarIndice resto (indice - 1) nuevoElem)

recorrerLista :: List a -> Int -> List a
recorrerLista Void _ = Void
recorrerLista (Node x lista) 0 = (Node x lista)
recorrerLista (Node x lista) indice = recorrerLista (insertarIndice lista (longitud lista) x) (indice - 1)