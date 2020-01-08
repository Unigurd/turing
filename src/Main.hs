import Prelude hiding (Left, Right, pred)
import Turing as T

data Symbol = A Integer | Heart | V | H | B deriving (Show, Eq)
data State  = Start | Q1 Integer | Q2 Integer | Q3 | Q4 | Stop deriving Eq

myPred = (/=Stop)
delta Start (A n) = ((Q1 n), V, Right)
delta Start Heart = (Q4, Heart, Right)
delta q@(Q1 _) a@(A _) = (q, a, Right)
delta (Q1 n) Heart = ((Q2 n), Heart, Right)
delta (Q2 n) (A m) = if n == m then (Q3, H, Left) else (Stop, (A m), Stay)
delta q@(Q2 _) H = (q, H, Right)
delta Q3 a@(A _) = (Q3, a, Left)
delta Q3 Heart   = (Q3, Heart, Left)
delta Q3 V       = (Start, V, Right)
delta Q3 H       = (Q3, H, Left)
delta Q4 H       = (Q4, H, Right)
delta _  symbol  = (Stop, symbol, Stay)

                                                    -- Difference is here        
                                                    --       |  
correctWord   = [A 1, A 2, A 3, A 4, A 5, Heart, A 1, A 2, A 3, A 4, A 5]
incorrectWord = [A 1, A 2, A 3, A 4, A 5, Heart, A 1, A 2, A 2, A 4, A 5]

myMaschine = T.Machine {emptySymbol = B, startState = Start, pred = myPred, fun =  delta}

main = do
  putStrLn $ show $ T.turing myMaschine correctWord
  putStrLn $ show $ T.turing myMaschine incorrectWord
