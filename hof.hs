-- | Nombres: Johanna Chan
-- |          Carlos Rodríguez
-- |
-- |  Carnés: 08-10218
-- |          06-40189
-- |
-- |--------------------------

--Funciones de Orden Superior (6 puntos)

--	Considere la función filter discutida en clase 
--		filter :: (a -> Bool) -> [a] -> [a]

--	Si bien la implantación de filter es directamente recursiva, 
--	y conveniente según la estrategia de evaluación de Haskell, 
--	se desea que Ud. implemente filter de maneras diferentes:

--		1. Usando listas por comprensión.
--			filterC = undefined 

			filterC :: (a->Bool) -> [a] -> [a]
			filterC f ls = [ x | x <- ls, f x]

--		2. Usando un map.
--			filterM = undefined

			filterM :: (a->Bool) -> [a] -> [a]
			filterM f [] = []
			filterM f ls = getTrues (zip (map f ls) ls)

			getTrues :: [(Bool,a)] -> [a]
			getTrues [] = []
			getTrues (x:xs) = if (fst x) 
				then ((snd x) : (getTrues xs))
				else getTrues xs

--		3. Usando un foldr:

			filterF :: (a -> Bool) -> [a] -> [a]
			filterF f [] = []
			filterF f ls = foldr (\ a b -> if f a then a:b else b) [] ls