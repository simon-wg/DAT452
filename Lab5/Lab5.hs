import System.IO

{--
A class representing the decision tree.
A thing of type QA is either a Branch 
containing a question and two other QA:s
or a Leaf containing an answer.
--}
data QA = Question String QA QA | Answer String
  deriving (Show,Read)

{--
Base tree for testing purposes
--}
baseTree = Question "Is this person from Europe?" 
              (Question "Is this person a scientist?" (Answer "Marie Curie") (Answer "Queen Elizabeth II")) 
              (Question "Is this person an actor" (Answer "Marilyn Monroe") (Answer "Hillary Clinton"))

-- | Function for traversing the question and answers
-- | in a tree by recursively asking a question and 
-- | and traversing the left QA when answered with 'y'
-- | or traversing the right QA when answered with 'n'.
-- | Otherwise recursively calls itself and asks the question again.
traverseTree :: QA -> IO QA
traverseTree (Question q qa1 qa2) = do 
    putStr ("\n" ++ q ++ " (y/n) ")
    hFlush stdout
    userAnswer <- getChar
    case userAnswer of
        'y' -> do
                q1 <- traverseTree qa1
                return (Question q q1 qa2)
        'n' -> do
                q2 <- traverseTree qa2 
                return (Question q qa1 q2)
        _   -> traverseTree (Question q qa1 qa2)

-- | If we have arrived at a person we ask whether the person
-- | we arrived at is the correct person. If so we simply return the tree.
-- | Otherwise we ask for the correct person and a question to distinguish
-- | between the two people and create a new Question from the data. 
traverseTree (Answer person) = do 
    putStr ("\nWas your person " ++ person ++ "? (y/n) ")
    userAnswer <- getChar
    case userAnswer of 
        'y' -> do
                putStr "\nYES - I WIN!\n"
                return (Answer person)
        'n' -> do 
                putStr ("\nOK -  you won this time.\n")
                putStr ("\nJust Curious: Who was your famous person? ")
                hFlush stdout
                newPerson <- getLine
                putStr ("Give me a question for which the answer for \"" 
                    ++ newPerson ++ "\" is \"yes\" and the answer for \"" 
                    ++ person ++ "\" is \"no\": ")
                newQuestion <- getLine
                return (Question newQuestion (Answer newPerson) (Answer person))

-- | When given a tree, writes that tree to the "database" file
-- | for the stored tree
updateTree :: QA -> IO()
updateTree tree = do writeFile "question.qa" (show tree)

-- | Collects a tree from storage to be used in the game
getTree :: IO QA
getTree = do 
        f <- readFile "question.qa"
        let qa = read f
        return qa

{-
Game loop is as follows:
        1. Get the tree from storage.
        2. Traverse the tree and update the tree if necessary
        3. Store the possibly updated tree.
-}
main :: IO()
main = do 
        tree <- getTree
        newTree <- traverseTree tree
        updateTree newTree