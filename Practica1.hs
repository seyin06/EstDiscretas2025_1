distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos(x1,y1) (x2,y2)= sqrt((x2-x1)**2+(y2-y1)**2)

hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt(b**2+h**2)

pendiente :: (Float,Float) -> (Float,Float) -> Float
pendiente (x1,y1) (x2,y2)= (y2-y1)/(x2-x1)

raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = ((-b + sqrt(b**2 - 4*a*c)) / (2*a), 
     (-b - sqrt(b**2 - 4*a*c)) / (2*a))

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo x y z = sqrt(((x+y+z)/2)*(((x+y+z)/2)-x)*(((x+y+z)/2)-y)*(((x+y+z)/2)-z))

comparador :: Int -> Int -> Int
comparador x y = if x == y
then 0
else if x>y
then 1
else -1

maximo :: Int -> Int -> Int -> Int
maximo x y z = if x>y && x>z
then x
else if y>z
then y
else z

esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w = if x >= y
then if y >= z
then if z >= w
then True
else False
else False
else False