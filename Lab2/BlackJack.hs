module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- A0 Writing out the sequence

size :: Num a => Hand -> a 
size Empty = 0 
size (Add card hand) = 1 + size hand


sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size (Empty)
            , 1 + 1 + 0
            ,2]

-- A1 Displaying a hand nicely

display :: Hand -> String
display Empty   = ""
display Add c h = (rank c) ++ "of" ++ (suit c)