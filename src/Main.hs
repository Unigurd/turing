import Prelude hiding (Left, Right, pred)
import Data.Foldable

data Step = Left | Stay | Right
data Tape a = Tape [a] a [a] deriving Eq
instance Foldable Tape where
  foldMap f (Tape ls elm rs) = foldMap f (reverse ls) `mappend` f elm `mappend` foldMap f rs

removeEmptySymbol emptySymbol [elm] = if emptySymbol == elm then [] else [elm]
removeEmptySymbol _           list  = list
remEmptSym :: Eq a => a -> [a] -> [a]
remEmptSym = removeEmptySymbol

move es Left  (Tape [] elm rs)       = Tape [] es (elm:(remEmptSym es rs))
move es Left  (Tape (l:ls) elm rs)   = Tape ls l (elm:(remEmptSym es rs))
move _  Stay  tape                   = tape
move es Right (Tape ls elm [])       = Tape (elm:(remEmptSym es ls)) es []
move es Right (Tape (ls) elm (r:rs)) = Tape (elm:(remEmptSym es ls)) r rs

change (Tape ls _ rs) newSymbol = (Tape ls newSymbol rs)

currentSymbol (Tape _ elm _) = elm

list2Tape (elm:rs) = Tape [] elm rs

turing :: Eq symbol
       => symbol
       -> state
       -> (state -> Bool)
       -> (state -> symbol -> (state, symbol, Step))
       -> [symbol]
       -> [symbol]
turing emptySymbol startState pred f rawTape = toList $ result
  where
    result = runTuring startState tape
    tape = list2Tape rawTape
    runTuring state tape = if pred newState 
                           then runTuring newState newTape
                           else newTape
      where
        (newState, newSymbol, step) = f state $ currentSymbol tape
        newTape = move emptySymbol step $ change tape newSymbol



data Symbol = A Integer | Heart | V | H | B deriving (Show, Eq)
data State  = Start | Q1 Integer | Q2 Integer | Q3 | Q4 | Stop deriving Eq

pred = (/=Stop)
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

myMaschine = turing B Start pred delta
                                                    -- Difference is here        
                                                    --       |  
correctWord   = [A 1, A 2, A 3, A 4, A 5, Heart, A 1, A 2, A 3, A 4, A 5]
incorrectWord = [A 1, A 2, A 3, A 4, A 5, Heart, A 1, A 2, A 2, A 4, A 5]

main = do
  putStrLn $ show $ myMaschine correctWord
  putStrLn $ show $ myMaschine incorrectWord