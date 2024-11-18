module BlackJack where

import Cards
import RunGame
import System.Random
import Test.QuickCheck

-- A0 Writing out the sequence

hand2 :: Hand
hand2 =
  Add
    (Card (Numeric 2) Hearts)
    (Add (Card Jack Spades) Empty)

sizeSteps :: [Integer]
{- What we do is essentially count cards one by one,
and removing the counted card from the hand -}
sizeSteps =
  [ size hand2,
    size
      ( Add
          (Card (Numeric 2) Hearts)
          (Add (Card Jack Spades) Empty)
      ),
    1 + size (Add (Card Jack Spades) Empty),
    1 + 1 + size Empty,
    1 + 1 + 0,
    2
  ]

-- A1 Displaying a hand nicely

{- Takes a card as a parameter, and returns a string
describing the rank and suit of it. -}
displayCard :: Card -> String
displayCard (Card (Numeric i) s) = show i ++ " of " ++ show s
displayCard (Card r s) = show r ++ " of " ++ show s

{- Takes a hand as a parameter, and returns a string
describing the rank and suit of all cards in it. -}
display :: Hand -> String
display Empty = ""
display (Add c h) = displayCard c ++ "\n" ++ display h

-- A2 Calc hand value

{- Takes a rank as a parameter, and returns the Integer value of it -}
valueRank :: Rank -> Integer
valueRank (Numeric i) = i
valueRank Ace = 11
valueRank r = 10

{- Takes a hand as a parameter, and returns the Integer value of it,
counting aces as 11 -}
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add c h) = valueRank (rank c) + initialValue h

{- Takes a hand as a parameter, and returns the Integer number of aces in it -}
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add c h)
  | rank c == Ace = 1 + numberOfAces h
  | otherwise = numberOfAces h

{- Takes a hand as a parameter, and returns the Integer value of it,
counting aces as 11 or 1 depending on if the player would go bust. -}
value :: Hand -> Integer
value Empty = 0
value (Add c h)
  | initVal <= 21 = initVal
  | otherwise = initVal - numberOfAces (Add c h) * 10
  where
    initVal = initialValue (Add c h)

-- A3 Is the player bust?

{- Takes a hand as a parameter
and returns whether or not it is greater than 21-}
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- A4 Winner function

{- Takes two hands as parameters, where the first is the guest,
and the second the bank. Returns which Player wins. -}
winner :: Hand -> Hand -> Player
winner guestHand bankHand
  | gameOver guestHand = Bank
  | gameOver bankHand = Guest
  | value guestHand <= value bankHand = Bank
  | otherwise = Guest

-- B1 Stack first hand on second

{- Takes two hands and returns one hand with the second argument representing
the top and the first representing the bottom -}
(<+) :: Hand -> Hand -> Hand
h1 <+ Empty = h1
h1 <+ (Add c h2) = Add c (h1 <+ h2)

{- Function from the instructions -}
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

{- Takes two hands, and asserts that the size of h1 <+ h2 equals
the sum of their individual sizes -}
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 =
  toInteger (size h1) + toInteger (size h2) == toInteger (size (h1 <+ h2))

-- B2 Full deck of cards
{-
ranks = Ace:King:Queen:Jack:[Numeric x | x <- [10,9..2]]
suits = [Spades, Diamonds, Clubs, Hearts]
[Add (Card y x) Empty | x <- suits, y <- ranks]
-}

allRanks = [Numeric x | x <- [10, 9 .. 2]] ++ [Jack, Queen, King, Ace]

allSuits = [Hearts, Clubs, Diamonds, Spades]

fullDeck =
  foldr
    (<+)
    Empty
    [ Add (Card y x) Empty
      | x <- allSuits,
        y <- allRanks
    ]

-- B3 Given a deck and a hand, draw one card from the deck and put on the hand.

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty h1 = error "draw: The deck is empty."
draw deck h1 = (restDeck, Add drawn h1)
  where
    Add drawn restDeck = deck

-- B4 Play deck
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand
  | value hand < 16 = playBankHelper deck' hand'
  | otherwise = hand
  where
    (deck', hand') = draw deck hand

-- B5 Shuffle deck

shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck = shuffleDeckHelper Empty

shuffleDeckHelper :: Hand -> StdGen -> Hand -> Hand
shuffleDeckHelper shuffled _ Empty = shuffled
shuffleDeckHelper shuffled g unshuffled =
  shuffleDeckHelper (shuffled <+ Add picked1 Empty) g1 newUnshuffled1
  where
    ((picked1, newUnshuffled1), g1) = randomCard g unshuffled

randomCard :: StdGen -> Hand -> ((Card, Hand), StdGen)
randomCard g deck = ((c, h), g1)
  where
    ((c, h), g1) = cardPicker (randomR (0, size deck - 1) g) deck

cardPicker :: (Int, StdGen) -> Hand -> ((Card, Hand), StdGen)
cardPicker (n, g) Empty = error "Cant pick card from empty deck"
cardPicker (n, g) deck = (cardPickerHelper n Empty deck, g)

cardPickerHelper :: Int -> Hand -> Hand -> (Card, Hand)
cardPickerHelper n leftDeck rightDeck
  | n < 0 || n >= size rightDeck = error "Index out of bounds"
  | n == 0 = (c, h <+ leftDeck)
  | otherwise = cardPickerHelper (n - 1) (Add c Empty <+ leftDeck) h
  where
    Add c h = rightDeck

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size (shuffleDeck g h) == size h

-- B6 - interface
implementation =
  Interface
    { iFullDeck = fullDeck,
      iValue = value,
      iDisplay = display,
      iGameOver = gameOver,
      iWinner = winner,
      iDraw = draw,
      iPlayBank = playBank,
      iShuffle = shuffleDeck
    }

main :: IO ()
main = runGame implementation