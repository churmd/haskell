module AcceptHeader where
import Control.Exception (assert)

data AcceptHeader = AcceptHeader {
  mimeType :: String,
  mineSubType :: String,
  weight :: String,
  version :: Maybe String,
  format :: Maybe String
} deriving (Eq, Show)

split :: (Eq a) => [a] -> a -> [a] -> [[a]]
split [] _ acc = [acc]
split (x:xs) y acc | x == y = [acc] ++ (split xs y [])
                   | otherwise = split xs y (acc ++ [x])

splitAcceptList :: String -> [String]
splitAcceptList str = split str ',' []

splitSingleAccept :: String -> (String, [String])
splitSingleAccept accept = (head splitAccept, tail splitAccept)
  where
    splitAccept = split accept ';' []

createOptionsMap :: [[String]] -> [(String, String)]
createOptionsMap [] = []
createOptionsMap (x:xs) =
  assert (length x == 2) $
  [(x!!0, x!!1)] ++ createOptionsMap xs

stripStart :: String -> String
stripStart (' ':xs) = stripStart xs
stripStart xs = xs

stripEnd :: String -> String
stripEnd str = reverse $ stripStart $ reverse str

strip :: String -> String
strip str = stripEnd $ stripStart str

parseAcceptHeader :: String -> AcceptHeader
parseAcceptHeader str =
  assert (length mimes == 2) $
  AcceptHeader (mimes!!0) (mimes!!1) weight version format
  where
    (mime, options) = splitSingleAccept str
    mimes = split mime '/' []
    optionsMap = createOptionsMap $ map (\x -> split x '=' []) $ map strip options
    weight = case lookup "q" optionsMap of
              Just x -> x
              Nothing -> "1"
    version = lookup "version" optionsMap
    format = lookup "format" optionsMap
