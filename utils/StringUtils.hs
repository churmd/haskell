module StringUtils (
) where

import ListUtils

subString :: String -> String -> Int
subString s subS = ListUtils.indexOfSubList s subS
