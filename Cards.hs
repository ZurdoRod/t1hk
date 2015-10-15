-- | Nombres: Johanna Chan
-- |          Carlos Rodríguez
-- |
-- |  Carnés: 08-10218
-- |          06-40189
-- |
-- |--------------------------

module Cards where

	data Card = Card { value :: Value, suit :: Suit }

	data Suit = Clubs | Diamonds | Spades | Hearts
	
	data Value = Numeric Int | Jack | Queen | King | Ace

	newtype Hand = H [Card]

	empty :: Hand
	empty = H []

	size :: Hand -> Int
	size (H []) 	= 0
	size (H (_:xs)) = 1 + size (H xs)


