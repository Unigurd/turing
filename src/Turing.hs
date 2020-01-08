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

-- The tape, with the current symbol between the left and right symbols
data Tape a = Tape [a] a [a] deriving Eq
instance Foldable Tape where
  foldMap f (Tape ls elm rs) = foldMap f (reverse ls) `mappend` f elm `mappend` foldMap f rs


removeEmptySymbol emptySymbol [elm] = if emptySymbol == elm then [] else [elm]
removeEmptySymbol _           list  = list
remEmptSym :: Eq a => a -> [a] -> [a]
remEmptSym = removeEmptySymbol

-- Move the position on the tape, cleaning up trailing/leading empty symbols
move es Left  (Tape [] elm rs)       = Tape [] es (elm:(remEmptSym es rs))
move es Left  (Tape (l:ls) elm rs)   = Tape ls l (elm:(remEmptSym es rs))
move _  Stay  tape                   = tape
move es Right (Tape ls elm [])       = Tape (elm:(remEmptSym es ls)) es []
move es Right (Tape (ls) elm (r:rs)) = Tape (elm:(remEmptSym es ls)) r rs

-- Change the symbol under the head
change (Tape ls _ rs) newSymbol = (Tape ls newSymbol rs)

-- Get current symbol
currentSymbol (Tape _ elm _) = elm

-- Convert list to tape
list2Tape (elm:rs) = Tape [] elm rs

-- Takes a turing machine and a tape, and returns the resulting tape and state of machine
turing :: Eq symbol => Machine state symbol -> [symbol] -> (state,[symbol])
turing machine rawTape = (endState, toList $ endTape) -- Convert the tape to [Symbol] before returning
  where
    -- The results
    (endState, endTape) = runTuring state tape
    -- converts the tape from [symbol] to Tape symbol
    tape   = list2Tape rawTape
    -- Gets the different things out of the machine
    state  = startState machine
    eSym   = emptySymbol machine
    p      = pred machine
    f      = fun machine
    -- If the predicate is false, the machine is in an end state and we thus return.
    -- Otherwise we continue
    runTuring state tape = if p newState 
                           then runTuring newState newTape
                           else (newState,newTape)
      where
        (newState, newSymbol, step) = f state $ currentSymbol tape
        newTape = move eSym step $ change tape newSymbol
