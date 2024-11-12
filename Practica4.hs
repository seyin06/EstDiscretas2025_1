data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving (Eq,Show)

longitud :: Arbol a -> Int
longitud ArbolVacio = 0
longitud (Raiz _ izquierdo derecho) = 1 + longitud izquierdo + longitud derecho

profundidad :: Arbol a -> Int
profundidad ArbolVacio = 0
profundidad (Raiz _ izquierdo derecho)= 1 + max (profundidad izquierdo)(profundidad derecho)

ancho :: Arbol a -> Int
ancho ArbolVacio = 0
ancho (Raiz _ ArbolVacio ArbolVacio) = 1
ancho (Raiz _ izquierdo derecho) = ancho izquierdo + ancho derecho

niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz x izquierdo derecho) = [x] : combinarNiv (niveles izquierdo) (niveles derecho)
    where
    combinarNiv [] ys = ys
    combinarNiv xs [] = xs
    combinarNiv (x:xs) (y:ys) = (x ++ y) : combinarNiv xs ys

minimo :: Ord a => Arbol a -> a
minimo ArbolVacio = error "Arbol vacio"
minimo (Raiz x ArbolVacio ArbolVacio) = x
minimo (Raiz x izquierdo derecho) = if x <= minimo izquierdo && x <= minimo derecho
                                    then x
				     				else if minimo izquierdo < minimo derecho
				     					then minimo izquierdo
				     					else minimo derecho

maximo :: Ord a => Arbol a -> a
maximo ArbolVacio = error "Arbol vacio"
maximo (Raiz x ArbolVacio ArbolVacio) = x
maximo (Raiz x izquierdo derecho) = if x >= maximo izquierdo && x >= maximo derecho
                                    then x
									else if maximo izquierdo > maximo derecho
										then maximo izquierdo
										else maximo derecho

eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio _ = error "Arbol vacio"
eliminar (Raiz x izquierda derecha) valor = if valor < x
                                            then Raiz x (eliminar izquierda valor) derecha
					     					else if valor > x
					     						then Raiz x izquierda (eliminar derecha valor)
					     						else if izquierda == ArbolVacio
					     							then derecha
					     							else if derecha == ArbolVacio
					     								then izquierda
					     								else Raiz minValor izquierda nuevoDer
					        							where
														minValor = minimo derecha
														nuevoDer = eliminar derecha minValor

data Recorrido = InOrder | PreOrder | PostOrder deriving (Eq, Show)

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz x izquierdo derecho) orden = if orden == InOrder
                                            then recorrido izquierdo InOrder ++ [x] ++ recorrido derecho InOrder
											else if orden == PreOrder
					      						then [x] ++ recorrido izquierdo PreOrder ++ recorrido derecho PreOrder
					      						else if orden == PostOrder
					      							then recorrido izquierdo PostOrder ++ recorrido derecho PostOrder ++ [x]
					      							else []
