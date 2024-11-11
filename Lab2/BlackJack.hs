module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- A0 Writing out the sequence

hand2 = (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))

sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size (Empty)
            , 1 + 1 + 0
            ,2]

-- A1 Displaying a hand nicely

displayCard :: Card -> String
displayCard (Card (Numeric i) s) = show i ++ " of " ++ show s
displayCard (Card r s)           = show r ++ " of " ++ show s 

display :: Hand -> String
display Empty   = ""
display (Add c h) = displayCard (c) ++ "\n" ++ display h

-- A2 Calc hand value

valueRank :: Rank -> Integer
valueRank (Numeric i) = i
valueRank Ace         = 11
valueRank r           = 10

initialValue :: Hand -> Integer
initialValue Empty     = 0
initialValue (Add c h) = valueRank (rank c) + initialValue h

numberOfAces :: Hand -> Integer
numberOfAces Empty     = 0
numberOfAces (Add c h)
    | rank c == Ace = 1 + numberOfAces h 
    | otherwise     =     numberOfAces h

value :: Hand -> Integer
value Empty     = 0
value (Add c h)
   | initVal <= 21 = initVal
   | otherwise     = initVal - ((numberOfAces (Add c h)) * 10)
   where initVal = initialValue (Add c h)
        

-- A3 Is the player bust?

gameOver :: Hand -> Bool
gameOver h 
   | value h > 21 = True
   | otherwise    = False

-- A4 Winner function

winner :: Hand -> Hand -> Player