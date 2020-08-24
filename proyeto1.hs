--1.  Programá las siguientes funciones

esCero :: Int -> Bool
esCero 0 = True
esCero n | n == 0 = True
	 	 | n /= 0 = False

esPositivo :: Int -> Bool
esPositivo n | n > 0 = True
	         | otherwise = False

esVocal:: Char -> Bool
esVocal x | (x =='a')|| (x =='e')|| (x =='i') || (x == 'o') || (x =='u') == True
	      | otherwise = False

--2.  Program ́a las siguientes funciones usando recursión o composición:
paratodo :: [Bool] -> Bool
paratodo [] = False
paratodo (x:xs) = x && paratodo xs

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs)= x + sumatoria xs

productoria :: [Int] -> Int
productoria [] = 0
productoria (x:xs) = 1 * productoria xs

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = (sumatoria xs) `div` length (x:xs)

--3.Program ́a la funci ́onpertenece :: Int -> [Int] -> Bool, que verifica si un n ́umero seencuentra en una lista.
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = (n == x) || pertenece n xs

--4.Program ́a las siguientes funciones que implementan los cuantificadores generales. Notá que el segundo par ́ametro de cada funci ́on, es otra funci ́on!
paratodo':: [a] -> (a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t = t x && paratodo' xs t

existe':: [a] -> (a -> Bool) -> Bool
existe' [] t = False
exite' (x:xs) t = t x || existe' xs t

sumatoria’ :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = t x + sumatoria xs t

productoria’ :: [a] -> (a -> Int) -> Int
productoria' [] t = 0
productoria (x:xs) t = t x * productoria xs t

--5.Defin ́ı nuevamente la funci ́onparatodo, pero esta vez usando la funci ́onparatodo’(sinrecursi ́on ni an ́alisis por casos!)
paraTodo xs = paratodo' xs id

--6.Utilizando las funciones del ejercicio 4, program ́a las siguientes funciones por composici ́on,sin usar recursi ́on ni an ́alisis por casos
todosPares = [Int] -> Bool
todosPares xs = (paratodo' xs) (\x -> mod x 2 == 0 )

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n x = (existe'xs) (\x -> mod x n == 0)

sumaCuadrados :: Int -> Int
sumacuadrados n = sumatoria' x [1..n] r
		  where r x = x * x
factorial' :: Int -> Int
factorial' n = factorial n

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria xs (filter (\x -> mod x 2 == 0) xs)

--7
--A)
-- FILTER:
--Una función de filter es aquella que dada una lista devuelve otra lista cuyos elementos son los elementos
--de la primera que cumplan una determinada condición, en el mismo orden y con las mismas repeticiones
--(si las hubiere). Por ejemplo: soloPares : [Int] → [Int] devuelve aquellos elementos de la lista que son
--pares.


-- MAP:
--Una función de map es aquella que dada una lista devuelve otra lista cuyos elementos son los que se
--obtienen de aplicar una funci´on a cada elemento de la primera en el mismo orden y con las mismas
--repeticiones (si las hubiere). Por ejemplo: duplica : [Int] → [Int] devuelve cada elemento de la lista
--multiplicado por 2.

--- ¿A qu´e equivale la expresi´on map succ [1, -4, 6, 2, -8], donde succ n = n+1?
--en esta lista el maple aplica a cada elemento succ(osea que a cada elemento de la lista le suma +1) y devuelve: [2,-3,7,3,-7]

-- ¿Y la expresi´on filter esPositivo [1, -4, 6, 2, -8]?
--en esta lista filter se encarga de ver en cada elemento de la lista si algun elemento es positivo, de ser asi se forma una
--nueva lista solo con los numeros positivos que tenia la anterior lista.

--8)Program ́a una funci ́on que dada una lista de n ́umerosxs, devuelve la lista que resulta deduplicar cada valor de xs.
--a)  Definila usando recursi ́on.
--b)  Definila utilizando la funci ́on map

duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (x * 2) : duplica xs

duplica' xs = map (*2) xs

--9.Program ́a una funci ́on que dada una lista de n ́umerosxs, calcula una lista que tiene comoelementos aquellos n ́umeros de xs que 
--son pares.
--a)  Definila usando recursi ́on.
--b)  Definila utilizando la funci ́onfilter.
--c)  Revis ́a tu definici ́on del ejercicio 6e. ¿C ́omo podes mejorarla?

Pares :: [Int] -> [Int]
Pares [] = 0
Pares (x:xs) | mod x 2 == 0 = x : pares xs
             | otherwise = pares xs

--10.La funci ́onprimIgualesAtoma un valor y una lista, y calcula el tramo inicial m ́as largo dela lista cuyos elementos son iguales a ese valor. Por ejemplo:primIgualesA 3 [3,3,4,1] = [3,3]primIgualesA 3 [4,3,3,4,1] = []primIgualesA 3 [] = []primIgualesA ’a’ "aaadaa" = "aaa"
--a)  Program ́a primIgualesA por recursi ́on.
--b)  Program ́a nuevamente la funci ́on utilizando takeWhile.

primIgualesA :: Eq(a)=> a -> [a] -> [a]
primIgualesA x [] = []

primIgualesA x (y:ys)| x == y : (primIgualesA x ys)
                     | x /= y = (primIgualesA x ys)   

primIgualesA' :: Eq(a)=> a -> [a] -> [a]
primIgualesA' x y = takeWhile (== x) y

--11).La funci ́on primIguales toma una lista y devuelve el mayor tramo inicial de la lista cuyoselementos son todos iguales entre si
--a)  Program ́aprimIgualespor recursi ́on.
--b)Us ́a cualquier versi ́on de primIgualesApara programarprimIguales. Est ́a permitidodividir en casos, pero no usar recursi ́on.
primIguales :: Eq(a) => [a] -> [a]
primIguales [] = []
primIguales [z] = [z]
primIguales (x:y:xs) | x == y = x : primIguales (y:xs)
                     | x /= y = [x]

primIguales' :: Eq(a) => [a] -> [a]
primIguales' (x:xs) = primIgualesA' x (x:xs) 

--12)






