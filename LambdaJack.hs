-- | Nombres: Johanna Chan
-- |          Carlos Rodríguez
-- |
-- |  Carnés: 08-10218
-- |          06-40189
-- |
-- |--------------------------

module LambdaJack where

	import Cards

	data Player = LambdaJack | You

	value :: Hand -> Int
	value (H xs) = foldr (\x b -> (extractNumeric x) + b) 0 xs where
																extractNumeric (Card (Numeric y) _) = y
																extractNumeric (Card w 			_)  | w == Ace  = 11
																								    | otherwise = 10

--	value2 :: Hand -> Int
--	value2 (H xs) = foldr (\x b -> (extractNumeric x) + b) 0 xs where
--																extractNumeric (Card (Numeric y) _) = y
--																extractNumeric (Card w 			_)  | w == Ace  = 1
--																								    | otherwise = 10


	busted :: Hand -> Bool
	busted x = if LambdaJack.value x > 21 then True else False

	winner :: Hand -> Hand -> Player
	winner x y | busted y = LambdaJack
			   | busted x && not (busted x) =  You
			   | LambdaJack.value x >= LambdaJack.value y = LambdaJack

	fullDeck :: Hand
	fullDeck = H $ [Card (Numeric x) y | x <- [1..10], y <- [Clubs, Diamonds, Spades, Hearts]] ++ [Card x y | x <- [Jack, Queen, King,Ace], y <- [Clubs, Diamonds, Spades, Hearts]]

	draw :: Hand -> Hand -> Maybe (Hand, Hand)
	draw (H []) _	   = Nothing
	draw (H xs) (H []) = Just (H (tail xs), H [head xs])
	draw (H xs) (H y)  = if LambdaJack.value (H y) > 21 then Nothing else Just (H (tail xs), H ((head xs):y))

	--playLambda :: Hand -> Hand
	playLambda (H xs) = playL (H xs) (H []) where
												playL (H xs) (H ys) = if LambdaJack.value (H ys) < 16 then playL (H (tail xs)) (H ((head xs):ys)) else (H ys)

