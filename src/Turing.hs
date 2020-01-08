module Turing (Step(Left, Stay, Right), Machine(Machine, emptySymbol, startState, pred, fun), Tape, turing) where
import Prelude hiding (Left, Right, pred)
import Data.Foldable

data Step = Left | Stay | Right
data Machine state symbol =
  Machine { emptySymbol :: symbol
          , startState  :: state
          , pred   :: (state -> Bool)
          , fun    :: (state -> symbol -> (state, symbol, Step)) 
          }
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

turing :: Eq symbol => Machine state symbol -> [symbol] -> [symbol]
turing machine rawTape = toList $ result
  where
    result = runTuring state tape
    tape = list2Tape rawTape
    state = startState machine
    eSym = emptySymbol machine
    p = pred machine
    f = fun machine
    runTuring state tape = if p newState 
                           then runTuring newState newTape
                           else newTape
      where
        (newState, newSymbol, step) = f state $ currentSymbol tape
        newTape = move eSym step $ change tape newSymbol
