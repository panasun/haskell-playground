module Word where

import Data.Monoid ((<>))

word :: String -> String
word w = "Hello " <> w <> " !"