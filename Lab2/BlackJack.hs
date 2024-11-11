module BlackJack where

import Cards
import RunGame
import Test.QuickCheck

-- A0 Writing out the sequence

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