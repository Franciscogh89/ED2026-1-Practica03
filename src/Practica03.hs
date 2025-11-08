module Practica03 where

-- Tipo de dato Prop
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)

-- Imprimir el tipo de dato Prop
instance Show Prop where
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p 
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Fórmulas proposicionales (Variables atómicas)
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Sinonimo para los estados
type Estado = [String]

-- Ejercicio 1
--Obtiene las variables de una formula proposicional sin duplicados
variables :: Prop -> [String]
variables (Cons True)  = []
variables (Cons False) = []
variables (Var p) =  [p]
variables (Not p) = variables p
variables (And p q) = eliminarDuplicados (variables p ++ variables q)
variables (Or p q) =  eliminarDuplicados (variables p ++ variables q)
variables (Impl p q) = eliminarDuplicados (variables p ++ variables q)
variables (Syss p q) =  eliminarDuplicados (variables p ++ variables q)

-- Ejercicio 2
--Evalua una forma proposicional dependiendo del estado dado
--(Si esta en la lista lo toma como true, si no como false)
interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons True)  _ = True
interpretacion (Cons False) _ = False
interpretacion (Var p) e = if contiene p e then True else False
interpretacion (Not p) e = not (interpretacion p e)
interpretacion (And p q) e = interpretacion p e && interpretacion q e
interpretacion (Or p q) e = interpretacion p e || interpretacion q e
interpretacion (Impl p q) e = not (interpretacion p e) || interpretacion q e
interpretacion (Syss p q) e = interpretacion p e == interpretacion q e

-- Ejercicio 3
--Genera todos los estados posibles para una formula proposicional (verdaderos y falsos)
estadosPosibles :: Prop -> [Estado]
estadosPosibles prop = conjuntoPotencia (variables prop)

-- Ejercicio 4
-- Obtener todos los modelos que satisfacen una fórmula
modelos :: Prop -> [Estado]
modelos prop = filtrar (\estado -> interpretacion prop estado) (estadosPosibles prop)

-- Ejercicio 5
-- Verificar si dos fórmulas son equivalentes
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes phi1 phi2 = todosIguales
  where
    -- Obtener todas las variables de ambas fórmulas
    todasVars = eliminarDuplicados (variables phi1 ++ variables phi2)
    -- Generar todos los estados posibles con esas variables
    todosEstados = conjuntoPotencia todasVars
    -- Verificar que ambas fórmulas tengan el mismo valor en todos los estados
    todosIguales = todos (\e -> interpretacion phi1 e == interpretacion phi2 e) todosEstados

-- Ejercicio 6
tautologia :: Prop -> Bool
tautologia p = if (numElem (estadosPosibles p) == numElem (modelos p)) then True else False

-- Ejercicio 7
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica ps p = tautologia (Impl (pegarBichos ps) p)

--Funcion auxiliar


--Funcion para ver que en elemnto este en la lisla
--usado en eliminarDuplicados (ejercicio 1)
--usado en interpretacion
contiene :: Eq a => a -> [a] -> Bool
contiene _ []  = False
contiene y (x:xs) = if x == y then True else contiene y xs


--Funcion para eliminar duplicados (de una lista)
--usado en el ejercicio 1 y 5
eliminarDuplicados :: Eq a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs)= if contiene x xs then eliminarDuplicados xs else x : eliminarDuplicados xs

-- Conjunto potencia
-- Usada en estadosPosibles (Ejercicio 3) y sonEquivalentes (Ejercicio 5)
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [(x:ys) | ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs

-- Función filter: filtra elementos de una lista que cumplen una condición
-- usada en modelos (Ejercicio 4)
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar condicion (x:xs) = if condicion x 
                           then x : filtrar condicion xs
                           else filtrar condicion xs

-- Función map: aplica una función a cada elemento de una lista
-- usada en algo?
mapear :: (a -> b) -> [a] -> [b]
mapear _ [] = []
mapear f (x:xs) = f x : mapear f xs

-- Función all: verifica que todos los elementos cumplan una condición
-- usada en sonEquivalentes (Ejercicio 5)
todos :: (a -> Bool) -> [a] -> Bool
todos _ [] = True
todos condicion (x:xs) = condicion x && todos condicion xs

-- Ejercicio 3 usada en Ejercicio 4, 6

-- Ejercicio 4 usada en Ejercicio 6

-- NumElem
-- Usada en tautologia (Ejercicio 6)
numElem:: [a] -> Int
numElem [] = 0
numElem (x:xs) = 1 + numElem xs

-- Pegar bichos
-- Crea una conjuncion a partir de los elementos de una lista de Prop
-- Usada en consecuenciaLogica (Ejercicio 7)
pegarBichos:: [Prop] -> Prop
pegarBichos [] = Cons True
pegarBichos (p:ps) = And p (pegarBichos ps)