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

--Funcion para ver que en elemnto este en la lisla
--usado en eliminarDuplicados (ejercicio 1)
--usado en interpretacion
contiene :: Eq a => a -> [a] -> Bool
contiene _ []  = False
contiene y (x:xs) = if x == y then True else contiene y xs


--Funcion para eliminar duplicados (de una lista)
--usado en el ejercicio 1
eliminarDuplicados :: Eq a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs)= if contiene x xs then eliminarDuplicados xs else x : eliminarDuplicados xs

-- Ejercicio 1
--Obtiene las variables de una formula proposicional sin duplicados
variables :: Prop -> [String]
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
modelos :: Prop -> [Estado]
modelos = undefined

-- Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes = undefined

-- Ejercicio 6
tautologia :: Prop -> Bool
tautologia = undefined

-- Ejercicio 7
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica = undefined

--Funcion auxiliar
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [(x:ys) | ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs