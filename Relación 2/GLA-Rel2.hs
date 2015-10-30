-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería del Software.
-- Alumno: GARCÍA LÓPEZ, ADRIÁN
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 2. Ejercicios resueltos: ..........
--
-------------------------------------------------------------------------------
import Test.QuickCheck

--EJERCICIO 1 (FALTAN COSAS)
--A
data Direction = North | South | East | West deriving (Eq, Ord, Enum, Show)

(<<) :: Direction -> Direction -> Bool
x << y = fromEnum x < fromEnum y


p_menor x y = (x < y) == (x << y)
instance Arbitrary Direction where
        arbitrary = do
                          n <- choose (0,3)
                          return $ toEnum n

--implementar con << or direccion == direccion2




--EJERCICIO 2

máximoYresto :: Ord a => [a] -> (a,[a])
máximoYresto (x:xs) = (maximum (x:xs), (filter quitar (x:xs)))
  where
    quitar y = y /= maximum (x:xs)

--EJERCICIO 3

reparte :: [a] -> ([a], [a])
reparte (x:xs) = (lista1 x (drop 1 xs), lista2 xs)
  where
   lista1 y [] = [y]
   lista1  y ys = [y] ++ lista1 (head ys) (drop 2 ys)

   lista2  [] = []
   lista2  (c:cs) =  [c] ++ lista2 (drop 1 cs)

--EJERCICIO 4

distintos :: Ord a => [a] -> Bool
distintos (x:xs) = not (comprueba (x:xs))
  where
    comprueba [_] = False
    comprueba (y:ys) = (or (map (\x -> x == y) ys)) || comprueba ys

--EJERCICIO 5


replicate' :: Int -> a -> [a]
replicate' x y =  [y | _ <- [1..x]]

p_replicate' n x = n >= 0 && n <= 1000 ==> length (filter (==x) xs) == n
                                            && length (filter (/=x) xs) == 0
                                              where
                                                xs = replicate' n x

--EJERCICIO 6

divideA :: Integer -> Integer -> Bool
divideA x y = mod y x == 0

divisores :: Integer -> [Integer]
divisores x = [z | z <- [1..x], divideA z x]

divisores' :: Integer -> [Integer]
divisores' x
          | x > 0       = [y | y <- [(-x)..(-1)], divideA y x] ++ [z | z <- [1..x], divideA z x]
          | x < 0       = [y | y <- [x..(-1)], divideA y x] ++ [z | z <- [1..(-x)], divideA z x]

--EJERCICIO 7

mcd :: Integer -> Integer -> Integer
mcd 0 y = 0
mcd x 0 = 0
mcd x y = mcd' (abs x) (abs y)
--mcd x y = maximum (interseccion (divisores x) (divisores y))
  where
    mcd' x y = maximum (interseccion (divisores x) (divisores y))
      where
        interseccion [] ys = []
        interseccion (m:ms) ys = (filter (==m) ys) ++ (interseccion ms ys)

p_mcd x y z = z /= 0 && (x,y) /= (0,0) ==> mcd (z*x) (z*y) == (abs z) * (mcd x y)
-- en el enunciado nos pone que puede ser x y z negativos pero la funcion mcd no esta definida para
--negativos ya que en el enunciado nos pone que nos ayudemos de la función divisores que solo acepta
-- positivos

mcm :: Integer -> Integer -> Integer
mcm x y = div (x*y) (mcd x y)

--EJERCICIO 8

esPrimo :: Integer -> Bool
esPrimo x = [y | y <- [1..x], mod x y == 0] == [1,x]

primosHasta :: Integer -> [Integer]
primosHasta x = [y | y <- [1..x], esPrimo y]

primosHasta' :: Integer -> [Integer]
primosHasta' x = filter (esPrimo) [1..x]

p1_primos x = primosHasta x == primosHasta' x

--EJERCICIO 9

  --posible implementación de esPrimo' para que acepte el 1 como primo
  -- y así que de goldbach 3 -> true
    --esPrimo :: Integer -> Bool
    --esPrimo x = [y | y <- [1..x], mod x y == 0] == [1,x]

pares :: Integer -> [(Integer, Integer)]
pares x = [(y,z) | y <- [1..(div x 2)], z <- [1..x], esPrimo y, esPrimo z, y + z == x]

goldbach :: Integer -> Bool
goldbach x =  if x == 3 then True
                        else not ( null (pares x))

goldbachHasta :: Integer -> Bool
goldbachHasta x = and [goldbach y | y <- [4,6..x]]

goldbachDébilHasta :: Integer -> Bool
goldbachDébilHasta x = or [ y == z + h + s | y <- [7,9..x], z <- primosHasta y, h <- primosHasta y, s <- primosHasta y ]

--EJERCICIO 10

esPerfecto :: Integer -> Bool
esPerfecto x = (foldr (+) 0 [ y | y <- [1..(x-1)], mod x y == 0 ]) == x

perfectosMenoresQue :: Integer -> [Integer]
perfectosMenoresQue x = [ y | y <- [1..x], esPerfecto y ]

--EJERCICIO 11

take' :: Int -> [a] -> [a]
take' n xs = [ x | (p,x) <- zip [0..(n-1)] xs ]

drop' :: Int -> [a] -> [a]
drop' n xs = [  x | (p,x) <- zip [1..(length xs)] xs, p > n ]

p_takedrop n xs = n >= 0 ==> (take' n xs) ++ (drop' n xs) == xs

--EJERCICIO 12

concat' :: [[a]] -> [a]
concat' xss = foldr (++) [] xss

concat'' :: [[a]] -> [a]
concat'' (xs : xss) = [ z | y <- (xs : xss), z <- y ]

--EJERICICIO 13

desconocida :: (Ord a) => [a] -> Bool
desconocida xs = and [ x<=y | (x,y) <- zip xs (tail xs) ]

--EJERCICIO 14

inserta :: (Ord a) => a -> [a] -> [a]
inserta x y = takeWhile (<x) y ++ [x] ++ dropWhile (<x) y

inserta' :: (Ord a) => a -> [a] -> [a]
inserta' k [h] = if k <= h then [k,h] else [h,k]
inserta' x (y:h:ys) = if x>=y && x<=h then [y] ++ [x] ++ [h] ++ ys
  else if (ys == [])
    then [h,x]
    else [y] ++ inserta' x (h:ys)

p1_inserta x xs = desconocida xs ==> desconocida (inserta x xs)

ordena :: (Ord a) => [a] -> [a]
ordena [] = []
ordena x = foldr (inserta) [] x

p2_inserta xs = desconocida (ordena xs)

--desconocida (ordena []) == True
--desconocida (ordena (xs)) = True
--desconocida (ordena (x:xs)) == True

-- PREGUNTAR A PEPE p3_inserta (x:xs) = desconocida (ordena (xs)) ==> desconocida (ordena (x:xs)) == True
