-- | Nombres: Johanna Chan
-- |          Carlos Rodríguez
-- |
-- |  Carnés: 08-10218
-- |          06-40189
-- |
-- |--------------------------

--Verificador de Tautologías (12 puntos)
--Representación de Expresiones
--Las expresiones de Lógica Proposicional de Primer Orden pueden
--definirse recursivamente como sigue:

--Una constante booleana True o False es una expresión de 
--Lógica Proposicional de Primer Orden.

--Una variable, representada por un String es una expresión de
--Lógica Proposicional de Primer Orden.

--La negación de una expresión de Lógica Proposicional de 
--Primer Orden, es una expresión de Lógica Proposicional de
--Primer Orden.

--La conjunción de dos expresiones de Lógica Proposicional
--de Primer Orden, es una expresión de Lógica Proposicional 
--de Primer Orden.

--La disyunción de dos expresiones de Lógica Proposicional
--de Primer Orden, es una expresión de Lógica Proposicional 
--de Primer Orden.

--La implicación de dos expresiones de Lógica Proposicional
--de Primer Orden, es una expresión de Lógica Proposicional 
--de Primer Orden.

--Defina un tipo recursivo monomórfico Haskell para representar--
--las expresiones de Lógica Proposicional. La definición del tipo
--debe incluir la cantidad mínima de instancias de clases de tipo
--necesarias para la funcionalidad que se solicita en el resto de
--este ejercicio.
  
			data Proposition = 
				Constante Bool 
				| Variable String 
				| Negacion Proposition 
				| Conjuncion Proposition Proposition 
				| Disjuncion Proposition Proposition 
				| Implicacion Proposition Proposition
				deriving (Show, Eq)

--Ambiente de Evaluación
--Para poder evaluar el valor de verdad de una proposición 
--particular es necesario contar con los valores de verdad 
--asociados a las variables involucradas. Esto se denomina el

--Ambiente de Evaluación y será modelado con un tipo de datos simple

			type Environment = [(String,Bool)]

--Escriba la función
--que determina si la variable k está definida en el ambiente e, 
--produciendo su valor booleano en caso afirmativo.


			find :: Environment -> String -> Maybe Bool

			find _ [] = Nothing
			find [] _ = Nothing
			find (x:xs) s = if fst x == s then Just (snd x) else find xs s 

--Escriba la función
--addOrReplace :: Environment -> String -> Bool -> Environment
--addOrReplace e k v = undefined

--tal que:
--Si en el ambiente e no existe ninguna asociación para la 
--variable k, la función produce un nuevo ambiente igual al
-- original pero agregando la asociación (k,v) al principio.

--Si en el ambiente e ya existe una asociación para la variable k,
--la función produce un nuevo ambiente igual al original 
--reemplazando la asociación existente por la nueva.

--Escriba la función


			add :: Environment-> String -> Bool-> Environment
			add (x:ys) s b
			    | s == fst x = (s,b):ys
			    | otherwise = x : add ys s b
			add ys s b = ys

			addOrReplace :: Environment -> String -> Bool -> Environment
			addOrReplace ls [] _    = ls
			addOrReplace [] s b     = [(s,b)]--
			addOrReplace ls s b = if find ls s == Nothing then (s,b):ls
										else add ls s b



--Escriba la función
--remove :: Environment -> String -> Environment
--remove e k = undefined
--tal que:
-- Si en el ambiente e no existe ninguna asociación para la variable k, la función
--produce el mismo ambiente sin modificar.
-- Si en el ambiente e existe una asociación para la variable k, la función produce
--un nuevo ambiente igual al original eliminando la asociación existente.

			remove :: Environment -> String -> Environment
			remove ls 		[] = ls
			remove [] 		_  = []
			remove (x : xs) s  = if fst x == s then xs else x : (remove xs s)



--Evaluando valores de verdad
--Teniendo esa función, podemos determinar el valor de verdad de una expresión arbitraria.
--Para ello, escriba la función
--evalP :: Environment -> Proposition -> Maybe Bool
--evalP e p = undefined

--que recorre la estructura recursiva de Proposition y se apoya en el ambiente e para
--determinar si la proposición p tiene un valor de verdad, calculándolo.
			
			extractMaybe :: Maybe Bool -> Bool
			extractMaybe Nothing  = False
			extractMaybe (Just x) = x

			evalP :: Environment -> Proposition -> Maybe Bool
			evalP _  (Constante 	x) = Just x
			evalP ls (Variable 		s) = find ls s
			evalP ls (Negacion 		x) = Just $ not (extractMaybe (evalP ls x))
			evalP ls (Conjuncion  x y) = Just $ extractMaybe (evalP ls x) && extractMaybe (evalP ls y)
			evalP ls (Disjuncion  x y) = Just $ extractMaybe (evalP ls x) || extractMaybe (evalP ls y)
			evalP ls (Implicacion x y) = Just $ not (extractMaybe (evalP ls x)) || extractMaybe (evalP ls y)



--Una Proposición es una Tautología si para todas las combinaciones de valores de verdad
--de las variables empleadas, siempre se evalúa al valor de verdad True.
--Comience por escribir la función
--vars :: Proposition -> [String]
--vars p = undefined

--que extrae los nombres de todas las variables presentes en la proposición p. Si una
--variable aparece más de una vez en la proposición, debe aparecer una sola vez en la
--lista.
			
			compress xs ys = foldr (\x b -> if (elem x b) then b else x : b) ys xs

			vars :: Proposition -> [String]
			vars (Constante 	x) = []
			vars (Variable 		s) = [s]
			vars (Negacion  	x) = vars x
			vars (Conjuncion  x y) = compress (vars x) (vars y)
			vars (Disjuncion  x y) = compress (vars x) (vars y)
			vars (Implicacion x y) = compress (vars x) (vars y)



--Escriba ahora la función
--isTautology :: Proposition -> Bool
--isTautology p = undefined

--tal que determine si la proposición p es una Tautología. El algoritmo general consiste
--en general todos los Ambientes de Evaluación resultantes de asignar todas las combi-
--naciones de valores de verdad a las variables presentes en la proposición, evaluarla, y
--determinar si siempre tiene valor de verdad True.

			aux :: [String] -> [Environment]
			aux ls = foldl (\xs b -> (map (\x -> (b,True):x) xs) ++ (map (\x -> (b,False):x) xs)) [[],[]] ls

			invertirEvalP :: Proposition -> Environment -> Bool
			invertirEvalP p e = extractMaybe $ evalP e p

			isTautology :: Proposition -> Bool
			isTautology p = foldr ((&&) . (invertirEvalP p)) True ((aux . vars) p)