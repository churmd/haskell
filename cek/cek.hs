import System.IO

data Or a b = Inl a | Inr b

type Name = String
type Lambda = (Name, Expr)
type Lit = Integer
type Env = [(Name, W)]
type Clos = (Lambda, Env)
data W = Lit Lit | Clos Clos
data Expr = Var Name | App Expr Expr | Lam Lambda | W W | Go Expr | Here Expr
data F = L W | R Expr Env | Arrow

exampleLambda :: Expr
exampleLambda = App (Lam ("x", (Var "x"))) (W (Lit 2))

lambdaToString :: Lambda -> String
lambdaToString (name, expr) = "(Lambda " ++ name ++ "." ++ show expr ++ ")"

envToString :: Env -> String
envToString [] = "0"
envToString ((name, w) : xs) = name ++ "->" ++ show w ++ "," ++ envToString xs

closToString :: Clos -> String
closToString (l, e) = "Clos(" ++ lambdaToString l ++ "," ++ envToString e ++ ")"

instance Show W where
  show (Lit l) = show l
  show (Clos c) = closToString c

instance Show Expr where
  show (Var name) = name
  show (App m1 m2) = "(" ++ show m1 ++ show m2 ++ ")"
  show (Lam l) = lambdaToString l
  show (W w) = show w
  show (Go expr) = "(go " ++ show expr ++ ")"
  show (Here expr) = "(here " ++ show expr ++ ")"

instance Show F where
  show (L w) = "(" ++ show w ++ " o)"
  show (R expr env) = "(o " ++ show expr ++ " " ++ envToString env ++ ")"
  show Arrow = ">>"

lookUpIn :: Name -> Env -> Maybe W
lookUpIn s [] = Nothing
lookUpIn s ((v, i) : xs) | s == v = Just i
                         | otherwise = lookUpIn s xs

type CEKState = (Expr, Env, [F])

framesToString :: [F] -> String
framesToString [] = "0"
framesToString (f : fs) = show f ++ ", " ++ framesToString fs

cekToString :: CEKState -> String
cekToString (expr, env, k) = "< " ++ show expr ++ " | " ++ envToString env ++ " | " ++ framesToString k ++ " >"

step :: CEKState -> Either String CEKState
step (Var x, e, k) = case lookUpIn x e of
                        Nothing -> Left "Syntax error : Couldn't find varibale in environment"
                        Just i -> Right (W i, e, k)
step (App m1 m2, e, k) = Right (m1, e, (R m2 e):k)
step (Lam l, e, k) = Right (W (Clos (l, e)), e, k)
step (W w, e, (R m e2 : k)) = Right (m, e2, (L w) : k)
step (W w, e, (L (Clos ((name, body), e2))) : k ) = Right (body, (name,w) : e2, k)
step (Here m, e, k) = Right (m, e, Arrow : k)
step (Go m, e, []) = Left "Syntax error : Unable to find corresponding here arrow (>>) for go"
step (Go m, e, (Arrow : k)) = Right (m, e, k)
step (Go m, e, (k : ks)) = Right (Go m, e, ks)
step (W w, e, (Arrow : k)) = Right (W w, e, k)
step (W (Lit x), e, []) = Left "Accepted input"
step _ = Left "Syntax error"

makeCEK :: Expr -> CEKState
makeCEK e = (e, [] , [])

cekMachine :: CEKState -> String
cekMachine cek = cekToString cek ++ "\n" ++ rest
  where
    rest = case step cek of
              Left s -> s ++ "\n"
              Right cek2 -> cekMachine cek2

interpret :: Expr -> IO ()
interpret e = putStr (cekMachine (makeCEK e))

interpret2 :: Expr -> IO ()
interpret2 e = loop (makeCEK e)
  where
    loop :: CEKState -> IO ()
    loop cek = do
      putStrLn (cekToString cek)
      case step cek of
        Left s -> putStrLn s
        Right cek2 -> loop cek2


exampleLambda2 :: Expr
exampleLambda2 = App (Lam ("f", Here (App (Lam ("x", W (Lit 1))) (App (Var "f") (W (Lit 2)))))) (Here (Lam ("y", Go (Var "y"))))
