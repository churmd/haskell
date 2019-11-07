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

type Value = Integer
type Child = Maybe Value
type CollatzGraph = Map.Map Value Child

-- Updates the child of a value. Child will always be the same unless it has not
-- been set.
updateChild :: Child -> Child -> Child
updateChild Nothing c2 = c2
updateChild c1 _ = c1

-- Add a node to the graph with its child value. Updates the child if the
-- node already exists.
addNode :: Value -> Child -> CollatzGraph -> CollatzGraph
addNode v c g = Map.insertWith updateChild v c g

-- Adds a collatz sequence to the given graph
addSequence :: [Value] -> CollatzGraph -> CollatzGraph
addSequence [] g = g
addSequence (x : []) g = addNode x Nothing g
addSequence (x : y : zs) g
  | Map.member y g = addChildYToX
  | otherwise = addSequence (y : zs) makeNodeYAfterX
    where
      addChildYToX = addNode x (Just y) g
      makeNodeYAfterX = addNode y Nothing addChildYToX

-- Retrives the collatz seqeunce starting with v from the graph if the sequence
-- is present
retrieveSeq :: Value -> CollatzGraph -> Maybe [Value]
retrieveSeq 1 graph = Just [1]
retrieveSeq v graph = do
  mChild <- Map.lookup v graph
  child <- mChild
  tailSeq <- retrieveSeq child graph
  return (v : tailSeq)

-- Creates an initial collatz graph with the node for the sequence {1,4,2}
baseCycle :: CollatzGraph
baseCycle = addSequence (collatzSequenceTillRepeat 1) (Map.singleton 1 Nothing)

-- Creates an initial collatz graph with the node for value 1 and no children
baseNoCycle :: CollatzGraph
baseNoCycle = Map.singleton 1 Nothing

-- Adds the collatz seqeunces initialised by 1 to n to the given graph
firstNCollatzSeqHelper :: Value -> Value -> CollatzGraph -> CollatzGraph
firstNCollatzSeqHelper current n graph
  | current > n = graph
  | otherwise = firstNCollatzSeqHelper (current + 1) n updatedGraph
    where
      updatedGraph = addSequence currentSeq graph
      currentSeq = collatzSequenceTo1 current

-- Creates a graph of the collatz sequences initialised by the values 1 to n
firstNCollatzSequences :: Value -> CollatzGraph
firstNCollatzSequences n = firstNCollatzSeqHelper 1 n baseCycle

-- Grpahs the first 1000 collatz sequences
first1000CollatzSequences :: CollatzGraph
first1000CollatzSequences = firstNCollatzSequences 1000

-- Returns the 800th collatz sequence if it is in the given graph
collatz800FromGraph :: CollatzGraph -> Maybe [Value]
collatz800FromGraph graph = do
  seq800 <- retrieveSeq 800 graph
  return seq800
