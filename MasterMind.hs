import System.Random
import Prelude hiding (read)
import Data.List (sort)
import Data.Char (toLower)
import Data.IORef

--Carga las funciones de generacion y el juego
empezarjuego = do wc; jugar

--Incia el juego
jugar = do
 putStrLn $ "Mastermind\nEscribe salir para salir del juego\n"
             ++ "Puedes usar los siguientes codigos para representar los colores de las fichas:\n"
			 ++ "Colores = Am | Az | Pl | Ne | Ca | Ve\n"
		     ++ "Coloca cada codigo separado por espacios son 4 en total"

 cuarteto <- obtenercodigo
--Llama a la funcion principal y le pasa el numero de intentos posibles antes de terminar el juego
 lazoprincipal cuarteto 8
--Una vez terminado el juego pregunta si desea terminar o reiniciar.
 putStrLn $ "\nEscribe si Para empezar de nuevo o no otra cosa para terminar\n"
 putStr "Salir Si/No "
 respuesta <- getLine
 if respuesta == "si" then jugar
	else if respuesta == "no" then putStrLn $ "Has decidido salir"
    else return ()

--Obtiene un cuarteto aleatorio del archivo generado anteriormente
obtenercodigo = do
   cuartetoDB <- readFile "cuarteto.txt"
   num <- randomRIO (0::Int, 1295) --
   let cuarteto = ((read cuartetoDB::[[Colores]]) !! num) --Toma un cuarteto del archivo entre 0 y 1295 por que existen ese numero de casos posibles
   return (cuarteto)

--El lazo del juego que se repite un numero de veces establecidos al llamar a la funcion
lazoprincipal cuarteto intentosposibles = do
   putStr "Ingresa Fichas "
   intento <- getLine --Se le pide ingresar fichas posibles
   let tester = (map (map fst) . sequence $ map reads (words intento) :: [[Colores]]) --convierte la entrada en un data set definido, en este caso colores
   let result = map read (words intento) :: [Colores]
   if intento == "s" || intento == "salir" || intento == "e" || intento == "exit" || intentosposibles == 0
      then putStrLn $ "Ha terminado el juego.\nEl cuarteto que no adivinaste es el siguiente: " ++ (show cuarteto)


      else if intento == "recordarcolores"
			then do putStrLn "Colores = Am | Az | Pl | Ne | Ca | Ve\n"
				lazoprincipal cuarteto intentosposibles

       else if tester == []
                         then do putStrLn "\nIncorrect input\n"
	                         lazoprincipal cuarteto intentosposibles
--Calcula el numero de fichas blancas y Negras que representan los aciertos y posiciones correctas
--Si acierta a la primera retorna que ha ganado
      else if (length cuarteto) > 4 || (length cuarteto) < 4 || (length (result)) < 4 || (length (result)) > 4
	                 then do putStrLn$ "Has ingresado mal el cuarteto por favor intenta nuevamente\n\n"
			         lazoprincipal cuarteto intentosposibles --lazo principal que se ejecuta luego de que ingresa algo

				else if (result) == cuarteto
				    then putStrLn $ "4 negras 0 blancas\n" ++ "Felicitaciones, Ganaste, el juego ha terminado"
			        else do putStrLn$ "\n" ++ show(numnegras cuarteto result) ++ " negras "
			      	                 		    ++ show (numblancas cuarteto result)
			      	                  		    ++ " blancas \nSigue intentando, Tienes " ++ show intentosposibles ++ " intentos restantes\n"
					lazoprincipal cuarteto (intentosposibles-1) --Llama al lazo principal y reduce en uno el intento posible


read :: Read a => [Char] -> a
read s           =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> x
                         []  -> error "Entrada Erronea"
                         _   -> error "Error"


--Enumeracion de los colores.
data Colores = Am | Az | Pl | Ne | Ca | Ve deriving (Eq, Ord, Show, Enum)

--instancia para admitir entradas en minusculas y convertirlas a mayusculas.
instance Read Colores where
    readsPrec _ (c1:c2:rest) = case lookup (map toLower [c1,c2])
        [("am",Am),("az",Az),("pl",Pl),("ne",Ne),("ca",Ca),("ve",Ve)]
      of Just c -> [(c,rest)]
         Nothing -> []

--Funcion que realiza todas las combinaciones posibles.
cuartetos = [ [a, b, c ,d] | a <- rs, b <- rs, c <- rs, d <- rs]
  where rs = [Am, Az, Pl, Ne, Ca, Ve]

--Escribe un archivo para generar los cuartetos posibles y luego sacar uno al azar
wc        =   do  writeFile "cuarteto.txt" (show cuartetos)
                  putStrLn "The Codes has been written!"

--Muestra el numero de combinaciones posibles.
movimientosposibles = putStrLn$ "Existen " ++ (show$ length cuartetos) ++ " movimientos posibles"

--Funcion que encuentra las fichas en la posicion correcta
negras :: [Colores] -> [Colores] -> [Colores]
negras [] [] = []
negras (x:xs) (y:ys)
			|(x == y) = x : (negras xs ys)
			|otherwise = negras xs ys

--Funcion que encuentra las fichas que se encuentran en el cuarteto pero estan en la posicion equivocada
w :: [Colores] -> [Colores] -> [Colores]
w x y = (blancas' (sort x) (sort y))
blancas' [] _ = []
blancas' _ [] = []
blancas' (x:xs) (y:ys)
  | (x == y) = x : (blancas' xs ys)
  | (x < y) = blancas' xs (y:ys)
  | (x > y) = blancas' ys (x:xs)

--Calculo del tama�o de las fichas negras
numnegras :: [Colores] -> [Colores] -> Int
numnegras xs ys = length (negras xs ys)

--Calculo del tama�o de las fichas blancas
numblancas :: [Colores] -> [Colores] -> Int
numblancas xs ys = length (w xs ys) - (numnegras xs ys)

--Funcion para generar un solo numero aleatorio
unaleatorio::Int->Int->Int
unaleatorio b s = (randomRs (1,b) (mkStdGen s)) !! (s `mod` b)

--Funcion para sacar dos individuos de mi poblacion
sacardos::[[Colores]] ->Int->Int ->([Colores],[Colores])
sacardos x s r =(a,b)
				where
				a=x !! s
				b=x !! r

--Funcion de algoritmo genetico: Mutacion
mutacion::[Colores] -> Colores -> Int -> [Colores]
mutacion [] _ _	= []
mutacion x r i	= w++[r]++c
				where
				w=fst (splitAt (i-1) x)
				c=tail (snd (splitAt (i-1) x))
--Mutar Poblacion
--mutacionP::[[Colores]] ->Int ->Int->[[Colores]]
--mutacionP (x:xs) i s	=(mutacion x p i):xs
--						where
--						p=unaleatorio 0 (s+1)

--Funcion de Cruzamiento en 1 punto
cruzaUnPunto::[Colores]->[Colores]->Int ->([Colores],[Colores])
cruzaUnPunto [] [] _ 	= ([],[])
cruzaUnPunto [] [a] _ 	= ([],[])
cruzaUnPunto [a] [] _	= ([],[])
cruzaUnPunto x y n		= (p,m)
					where
					m=(fst c)++(snd d)
					p=(fst d)++ (snd c)
					d=splitAt n x
					c=splitAt n y
--Funcion de Cruzamiento 2 puntos
cruzaDosPunto::[Colores]->[Colores]->Int->Int ->([Colores],[Colores])
cruzaDosPunto x y n l	=(p,m)
						where
						m=(fst f)++(snd c)
						p=(snd f)++(snd d)
						c=splitAt (n+l) x
						d=splitAt (n+l) y
						f=cruzaUnPunto (fst c) (fst d) n
						
--Funcion de Cruzar dos individuos aleatorios de una poblacion
cruzar::[[Colores]]->Int-> ([Colores],[Colores])
cruzar x s =((fst (cruzaUnPunto a b 10)),
			(snd (cruzaUnPunto a b 10)))
			where
			i= unaleatorio ((length x)-1) (s+6)
			j= unaleatorio ((length x)-1) (s+3)
			a= fst (sacardos x i j)
			b= snd (sacardos x i j)
			
--Funcion de CrossOver de toda la poblacion
crossover::[[Colores]]->Int-> [[Colores]]
crossover [] _	= []
crossover x s	= [a]++[b]++crossover x (s+10)
				where
				a=fst (cruzar x (s+3))
				b=snd (cruzar x (s+2))


--Obtiene una poblacon de s cuartetos hasta 0
poblacion::Int->[[Colores]]-> [[Colores]]
poblacion _ []	= []
poblacion 0 x	= [a]
				where
					a=x !! 0
poblacion s x	= [a]++poblacion (s-1) x
				where
					a=x !! (s-1)
									
--Escribe un archivo donde generamos la poblacion de 150 del algoritmo
pob        =   do  	writeFile "poblacion.txt" (show (poblacion 150 cuartetos))

--Genera individuos aleatorios
aleatoriosI:: Int -> (Int,Int) -> [Int]
aleatoriosI s (a,b)	= randomRs (a, b) (mkStdGen s)

--Funcion que genera un solo individuo para la poblacion
generaIndividuo:: Int -> Int ->(Int,Int)->[Int]
generaIndividuo n l (a,b) = take n (aleatoriosI l (a,b))

--Genera la poblacion param: numero de cuartetos, cantidad de colores, semilla, rango de colores
--Devuelve la poblacion de cuartetos
generaPoblacion:: Int ->Int->Int->(Int,Int)->[[Int]]
generaPoblacion 0 _ _ (a,b) = []
generaPoblacion n l p (a,b) = (generaIndividuo l (p-1) (a,b)): (generaPoblacion (n-1) (l) (p+10) (a,b))
