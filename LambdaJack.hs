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

	busted :: Hand -> Bool
	busted x = if LambdaJack.value x > 21 then True else False

	winner :: Hand -> Hand -> Player
	winner x y | busted y = LambdaJack
			   | busted x && not (busted x) =  You
			   | LambdaJack.value x >= LambdaJack.value y = LambdaJack


--	value :: Hand -> Int
--	value (H [Card (Numeric x) _]) = x
--	value (H [Card Jack _])		   = 10
--	value (H [Card Queen _])	   = 10
--	value (H [Card King _])		   = 10
--	value (H (x:xs))			   = LambdaJack.value (H [x]) + LambdaJack.value (H xs)
	-- Falta caso Ace




--data Card = Card { value :: Value, suit :: Suit }
--data Suit = Clubs | Diamonds | Spades | Hearts
--data Value = Numeric Int | Jack | Queen | King | Ace
--newtype Hand = H [Card]