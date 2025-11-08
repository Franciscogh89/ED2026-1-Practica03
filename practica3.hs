module LabDis where
-- Sintaxis de la lógica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

-- Para que se vea bien en la terminal
instance Show Prop where 
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Algunas variables para no estar escribiendo mucho en la terminal
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Tipo para representar un estado (asignación de valores de verdad)
type Estado = [String]
type Modelo = Estado

-- ============================================================
-- FUNCIONES AUXILIARES (ya existentes)
-- ============================================================

-- Función para ver que un elemento esté en la lista
-- usado en eliminarDuplicados (ejercicio 1)
-- usado en interpretacion
contiene :: Eq a => a -> [a] -> Bool
contiene _ []  = False
contiene y (x:xs) = if x == y then True else contiene y xs

-- Función para eliminar duplicados (de una lista)
-- usado en el ejercicio 1
eliminarDuplicados :: Eq a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs)= if contiene x xs then eliminarDuplicados xs else x : eliminarDuplicados xs

-- Ejercicio 1
-- Obtiene las variables de una fórmula proposicional sin duplicados
variables :: Prop -> [String]
variables (Cons _) = []
variables (Var p) =  [p]
variables (Not p) = variables p
variables (And p q) = eliminarDuplicados (variables p ++ variables q)
variables (Or p q) =  eliminarDuplicados (variables p ++ variables q)
variables (Impl p q) = eliminarDuplicados (variables p ++ variables q)
variables (Syss p q) =  eliminarDuplicados (variables p ++ variables q)

-- Ejercicio 2
-- Evalúa una forma proposicional dependiendo del estado dado
-- (Si está en la lista lo toma como true, si no como false)
interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons b) _ = b
interpretacion (Var p) e = if contiene p e then True else False
interpretacion (Not p) e = not (interpretacion p e)
interpretacion (And p q) e = interpretacion p e && interpretacion q e
interpretacion (Or p q) e = interpretacion p e || interpretacion q e
interpretacion (Impl p q) e = not (interpretacion p e) || interpretacion q e
interpretacion (Syss p q) e = interpretacion p e == interpretacion q e

-- Función filter: filtra elementos de una lista que cumplen una condición
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar condicion (x:xs) = if condicion x 
                           then x : filtrar condicion xs
                           else filtrar condicion xs

-- Función map: aplica una función a cada elemento de una lista
mapear :: (a -> b) -> [a] -> [b]
mapear _ [] = []
mapear f (x:xs) = f x : mapear f xs

-- Función all: verifica que todos los elementos cumplan una condición
todos :: (a -> Bool) -> [a] -> Bool
todos _ [] = True
todos condicion (x:xs) = condicion x && todos condicion xs

-- Función auxiliar para generar el conjunto potencia (todos los subconjuntos)
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = conjuntoPotencia xs ++ mapear (x:) (conjuntoPotencia xs)

-- Ejercicio 3
-- Genera todos los estados posibles para una fórmula proposicional (verdaderos y falsos)
estadosPosibles :: Prop -> [Estado]
estadosPosibles prop = conjuntoPotencia (variables prop)

-- ============================================================
-- FUNCIONES PRINCIPALES
-- ============================================================

-- Ejercicio 4
-- Obtener todos los modelos que satisfacen una fórmula
modelos :: Prop -> [Modelo]
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


