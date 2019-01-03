module TruthTable where
  import LogicExpressions

  type PlaceHolderExpr = Expr String
  type VarTable = [(String, Bool)]
  type ExprCombination = (PlaceHolderExpr, VarTable)

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

  substituteVars :: VarTable -> PlaceHolderExpr -> Maybe LogicExpr
  substituteVars vt (Var a) = lookUp a vt >>= (\b ->  return (Var b))
  substituteVars vt (Not a) = substituteVars vt a >>= \logExpr ->
                              return (Not logExpr)
  substituteVars vt (And a b) = substituteVars vt a >>= \logExprA ->
                                substituteVars vt b >>= \logExprB ->
                                return (And logExprA logExprB)
  substituteVars vt (Or a b) = substituteVars vt a >>= \logExprA ->
                                substituteVars vt b >>= \logExprB ->
                                return (Or logExprA logExprB)
  substituteVars vt (Xor a b) = substituteVars vt a >>= \logExprA ->
                                substituteVars vt b >>= \logExprB ->
                                return (Xor logExprA logExprB)  
