data Card = Card Rank Suit | Joker  deriving (Eq, Show)

data Rank = Numeric Integer | Jack | Queen | King | Ace
            deriving (Eq, Show)

data Suit = Hearts | Spades | Diamonds | Clubs
            deriving (Eq, Show)

data Hand = First Card | Add Card Hand   deriving (Eq, Show)

