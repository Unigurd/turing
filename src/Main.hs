import Prelude hiding (Left, Right, pred)
import Turing as T

-- This is a machine from some past assignment. Just to check my implementation
-- It verifies words of the form 
-- A1 A2 A3 ... An Heart A1 A2 A3 ... An
-- The numbers do not neccessarily have to come in numerical order
-- if the word fits, the tape will look like 
-- V  V  V  ... V  Heart H  H  H  ... H
-- after the computation has finished

-- Read the explanation for delta
data Symbol = A Integer | Heart | V | H | B deriving (Show, Eq)
data State  = Start | Q1 Integer | Q2 Integer | Q3 | Q4 | Failure | Finish deriving (Show, Eq)

-- The state Stop is the only stopping state
myPred state = foldl (\acc elm -> (elm /= state) && acc) True [Failure, Finish]

-- This function and explanation is organized according to the happy path in the computation
-- First we read some symbol An and write V to indicate we have been here
delta Start (A n) = ((Q1 n), V, Right)
-- Then we head left over all the symbols on the left side of the heart
delta q@(Q1 _) a@(A _) = (q, a, Right)
-- When reaching the heart we switch state
delta (Q1 n) Heart = ((Q2 n), Heart, Right)
-- and keeping heading right over all the H's,
-- which indicate (A n)'s we've already done.
-- There are no H's in the beginning
delta q@(Q2 _) H = (q, H, Right)
-- When we find the first A m on the right side of the heart,
-- we write a H if m is the same as the n we found in the beginning
-- (which we carried in our state).
-- If it isn't, we stop prematurely
delta (Q2 n) (A m) = if n == m then (Q3, H, Left) else (Failure, (A m), Stay)
-- We then start heading left over the H's
delta Q3 H         = (Q3, H, Left)
-- and over the heart
delta Q3 Heart    = (Q3, Heart, Left)
-- and over the untreated A's to the left of the heart.
delta Q3 a@(A _)   = (Q3, a, Left)
-- When we find the first V, we know everything to the left is done,
-- and so we prepare to repeat everything on the symbol to the right.
delta Q3 V         = (Start, V, Right)
-- If the first symbol we see when we repeat is the heart,
-- we only have left to check that there aren't any extra A's
-- on the right side
delta Start Heart  = (Q4, Heart, Right)
-- so we walk past all the H's
delta Q4 H         = (Q4, H, Right)
-- If the first non-H symbol we find is the blank symbol,
-- the given word has the right form.
delta Q4 B         = (Finish, B, Stay)
-- If anything else happens, the word is not of the correct form,
-- and we exit with failure.
delta _  symbol    = (Failure, symbol, Stay)


                                                    -- Difference is here        
                                                    --       |  
correctWord   = [A 1, A 2, A 3, A 4, A 5, Heart, A 1, A 2, A 3, A 4, A 5]
incorrectWord = [A 1, A 2, A 3, A 4, A 5, Heart, A 1, A 2, A 2, A 4, A 5]

myMaschine = T.Machine {emptySymbol = B, startState = Start, pred = myPred, fun =  delta}

main = do
  putStrLn "Should be correct:"
  putStrLn $ show $ T.turing myMaschine correctWord
  putStrLn "Should be incorrect:"
  putStrLn $ show $ T.turing myMaschine incorrectWord
