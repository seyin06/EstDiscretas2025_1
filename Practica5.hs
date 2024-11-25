import GHC.Exts.Heap (GenClosure(var))
data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y| y <- xs, y /= x]

variables :: Formula -> [Var]
variables (Atom var) = [var]
variables (Neg formula) = conjunto (variables formula)
variables (formula1 :&: formula2) = conjunto (variables formula1 ++ variables formula2)
variables (formula1 :|: formula2) = conjunto (variables formula1 ++ variables formula2)
variables (formula1 :=>: formula2) = conjunto (variables formula1 ++ variables formula2)
variables (formula1 :<=>: formula2) = conjunto (variables formula1 ++ variables formula2)


negacion :: Formula -> Formula
negacion (Atom var) = Neg (Atom var)
negacion (Neg formula) = formula
negacion (formula1 :&: formula2) = negacion formula1 :|: negacion formula2
negacion (formula1 :|: formula2) = negacion formula1 :&: negacion formula2
negacion (formula1 :=>: formula2) = formula1 :&: negacion formula2
negacion (formula1 :<=>: formula2) = (formula1 :&: negacion formula2) :|: (formula2 :&: negacion formula1)


equivalencia :: Formula -> Formula
equivalencia (Atom var) = Atom var
equivalencia (Neg(Atom var)) = Neg (Atom var)
equivalencia (Neg formula) = negacion (equivalencia formula)
equivalencia (formula1 :&: formula2) = equivalencia formula1 :&: equivalencia formula2
equivalencia (formula1 :|: formula2) = equivalencia formula1 :|: equivalencia formula2
equivalencia (formula1 :=>: formula2) = equivalencia (Neg formula1 :|: formula2)
equivalencia (formula1 :<=>: formula2) = equivalencia ((formula1 :&: formula2) :|: (Neg formula1 :&: Neg formula2))


interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion (Atom var) valores = valoresAux var valores
interpretacion (Neg formula) valores = if interpretacion formula valores
                                            then False
                                            else True
interpretacion (formula1 :&: formula2) valores = interpretacion formula1 valores && interpretacion formula2 valores
interpretacion (formula1 :|: formula2) valores = interpretacion formula1 valores || interpretacion formula2 valores
interpretacion (formula1 :=>: formula2) valores = if interpretacion formula1 valores
                                                        then interpretacion formula2 valores
                                                        else True
interpretacion (formula1 :<=>: formula2) valores = interpretacion formula1 valores == interpretacion formula2 valores

valoresAux :: Var -> [(Var, Bool)] -> Bool
valoresAux var [] = error "Una de las variables no esta definida"
valoresAux var ((v,valor):xs) = if var == v
                                    then valor
                                    else valoresAux var xs


combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones formula = if null (conjunto (variables formula))
                            then [[]]
                            else combinar (conjunto (variables formula))

combinar :: [Var] -> [[(Var,Bool)]]
combinar [] = [[]]
combinar (y:ys) = [(y,True) : z | z <- combinar ys] ++ [(y,False) : z | z <- combinar ys]


tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad formula = [(asignaciones, interpretacion formula asignaciones) | asignaciones <- combinaciones formula]
