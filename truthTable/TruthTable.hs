module TruthTable where
import LogicExpressions as LE
import Data.List as List

type PlaceHolderExpr = Expr String
type VarTable = [(String, Bool)]
type ExprCombination = (VarTable, PlaceHolderExpr)
type VarSolution = (VarTable, Maybe Bool)

extractVarNames :: PlaceHolderExpr -> [String]
extractVarNames (Var s) = [s]
extractVarNames (Not s) = extractVarNames s
extractVarNames (And a b) = extractVarNames a ++ extractVarNames b
extractVarNames (Or a b) = extractVarNames a ++ extractVarNames b
extractVarNames (Xor a b) = extractVarNames a ++ extractVarNames b

allPropositions :: [a] -> [[(a, Bool)]]
allPropositions xs = sequence $ map (\x -> [(x, True), (x, False)]) xs

lookUp :: (Eq a) => a -> [(a, b)] -> Maybe b
lookUp k [] = Nothing
lookUp k ((key, value):xs) | k == key = Just value
                           | otherwise = lookUp k xs

substituteVars :: ExprCombination -> Maybe LogicExpr
substituteVars (vt, (Var a)) =
  lookUp a vt >>= (\b ->  return (Var b))
substituteVars (vt, (Not a)) =
  substituteVars (vt, a) >>= \logExpr ->
  return (Not logExpr)
substituteVars (vt, (And a b)) =
  substituteVars (vt, a) >>= \logExprA ->
  substituteVars (vt, b) >>= \logExprB ->
  return (And logExprA logExprB)
substituteVars (vt, (Or a b)) =
  substituteVars (vt, a) >>= \logExprA ->
  substituteVars (vt, b) >>= \logExprB ->
  return (Or logExprA logExprB)
substituteVars (vt, (Xor a b)) =
  substituteVars (vt, a) >>= \logExprA ->
  substituteVars (vt, b) >>= \logExprB ->
  return (Xor logExprA logExprB)

solve :: PlaceHolderExpr -> [VarSolution]
solve expr = zip varTables (evaluateAllOptions varTables expr)
  where
    varTables = allPropositions $ extractVarNames expr

evaluateAllOptions :: [VarTable] -> PlaceHolderExpr -> [Maybe Bool]
evaluateAllOptions [] _ = []
evaluateAllOptions (vt:vts) expr =
  [evaluate (vt, expr)] ++ (evaluateAllOptions vts expr)

evaluate :: ExprCombination -> Maybe Bool
evaluate exprCom = substituteVars exprCom >>= \lExpr ->
                      return (eval lExpr)

tableString :: [VarSolution] -> String
tableString [] = ""
tableString varSolutions =
  nDashes totalLen ++ "\n" ++
  printTitle uniqueVars (maxVarLen, maxBoolLen)
  where
    (vts, mbs) = unzip varSolutions
    flattenVTS = concat vts
    (vars, bools) = unzip flattenVTS
    uniqueVars = List.nub vars
    numVars = length uniqueVars
    maxVarLen = maximum $ map (\s -> length s) (vars ++ ["True", "False"])
    maybeBoolStrings = map resultString mbs
    maxBoolLen = maximum $ map (\s -> length s) (maybeBoolStrings ++ ["Result"])
    totalLen = (maxVarLen * numVars) + numVars + maxBoolLen + 2

resultString :: Maybe Bool -> String
resultString (Just x) = show x
resultString Nothing = "NA"

nDashes :: Int -> String
nDashes n = take n $ repeat '-'

pad :: String -> Int -> String
pad s n | length s < n = s ++ (take (n - length s) $ repeat ' ')
        | otherwise = s

padAll :: [String] -> Int -> [String]
padAll [] _ = []
padAll (s:ss) n = [(pad s n)] ++ padAll ss n

printTitle :: [String] -> (Int, Int) -> String
printTitle vars (varLen, resultLen) = allVars ++ paddedResult ++ "|"
  where
    paddedVars = padAll vars varLen
    paddedResult = pad "Result" resultLen
    allVars = foldl (\acc s -> acc ++ s ++ "|") "|" paddedVars
