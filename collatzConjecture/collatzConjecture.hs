module Collatz where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

{--
The Collatz conjecture is based around a sequence where you start with any
positive integer n greater than zero. The next term in the sequence is obtained
by applying previous term to the function f.

Where f(n) is :
  - When n is even, return n/2
  - When n is odd, return 3n + 1

The conjecture is that no matter which initial n is chosen, the sequence will
reach 1. Once it reaches 1, the sequence will loop through the numbers {1,4,2}.
--}

-- Get the next term in the sequence given the previous term
nextNumber :: Integer -> Integer
nextNumber n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = (3 * n) + 1

-- Returns the a Collatz sequence terminating at 1 using an accumulator to build
-- the sequence
sequenceTo1Helper :: Integer -> [Integer] -> [Integer]
sequenceTo1Helper 1 acc = acc ++ [1]
sequenceTo1Helper n acc = sequenceTo1Helper (nextNumber n) (acc ++ [n])

-- Returns the collatz sequence starting from n until 1 is reached
collatzSequenceTo1 :: Integer -> [Integer]
collatzSequenceTo1 n
  | n <= 0 = error "Collatz sequence can only be started with a \
                    \positive integer greater than zero"
  | otherwise = sequenceTo1Helper n []

  -- Returns the a Collatz sequence terminating when a duplicate term appears
  -- using an accumulator to build the sequence. Includes the duplicate.
sequenceTillRepeatHelper :: Integer -> [Integer] -> [Integer]
sequenceTillRepeatHelper n acc
  | elem n acc = acc ++ [n]
  | otherwise = sequenceTillRepeatHelper (nextNumber n) (acc ++ [n])

-- Returns the collatz sequence starting from n until a duplicate term appears.
-- Includes the duplicate.
collatzSequenceTillRepeat :: Integer -> [Integer]
collatzSequenceTillRepeat n
  | n <= 0 = error "Collatz sequence can only be started with a \
                    \positive integer greater than zero"
  | otherwise = sequenceTillRepeatHelper n []

-- A map strucure to hold multiple sequences to prevent re-calculating
-- existing parts of sequences.
-- Each key in the cMap will be the current number in the sequence n.
-- The key will then have a child which is the next number in the sequence, if it is known
type Value = Integer
type Child = Maybe Value
type CollatzMap = Map.Map Value Child

-- Updates the child of a value. Child will always be the same unless it has not
-- been set.
updateChild :: Child -> Child -> Child
updateChild Nothing c2 = c2
updateChild c1 _ = c1

-- Add a node to the cMap with its child value. Updates the child if the
-- node already exists.
addNode :: Value -> Child -> CollatzMap -> CollatzMap
addNode v c cMap = Map.insertWith updateChild v c cMap

-- Adds a collatz sequence to the given cMap
addSequence :: [Value] -> CollatzMap -> CollatzMap
addSequence [] cMap = cMap
addSequence (x : []) cMap = addNode x Nothing cMap
addSequence (x : y : zs) cMap
  | Map.member y cMap = addChildYToX
  | otherwise = addSequence (y : zs) makeNodeYAfterX
    where
      addChildYToX = addNode x (Just y) cMap
      makeNodeYAfterX = addNode y Nothing addChildYToX

-- Retrives the collatz seqeunce starting with v from the cMap if the sequence
-- is present
retrieveSeq :: Value -> CollatzMap -> Maybe [Value]
retrieveSeq 1 cMap = Just [1]
retrieveSeq v cMap = do
  mChild <- Map.lookup v cMap
  child <- mChild
  tailSeq <- retrieveSeq child cMap
  return (v : tailSeq)

-- Creates an initial collatz cMap with the node for the sequence {1,4,2}
baseCycle :: CollatzMap
baseCycle = addSequence (collatzSequenceTillRepeat 1) (Map.singleton 1 Nothing)

-- Creates an initial collatz cMap with the node for value 1 and no children
baseNoCycle :: CollatzMap
baseNoCycle = Map.singleton 1 Nothing

-- Adds the collatz seqeunces initialised by 1 to n to the given cMap
firstNCollatzSeqHelper :: Value -> Value -> CollatzMap -> CollatzMap
firstNCollatzSeqHelper current n cMap
  | current > n = cMap
  | otherwise = firstNCollatzSeqHelper (current + 1) n updatedcMap
    where
      updatedcMap = addSequence currentSeq cMap
      currentSeq = collatzSequenceTo1 current

-- Creates a cMap of the collatz sequences initialised by the values 1 to n
firstNCollatzSequences :: Value -> CollatzMap
firstNCollatzSequences n = firstNCollatzSeqHelper 1 n baseCycle

-- Grpahs the first 1000 collatz sequences
first1000CollatzSequences :: CollatzMap
first1000CollatzSequences = firstNCollatzSequences 1000

-- Returns the 800th collatz sequence if it is in the given cMap
collatz800FromCMap :: CollatzMap -> Maybe [Value]
collatz800FromcMap cMap = do
  seq800 <- retrieveSeq 800 cMap
  return seq800
