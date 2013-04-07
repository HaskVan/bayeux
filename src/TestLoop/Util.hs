module TestLoop.Util where

import           Data.List                           (intersperse)

-- Utils -----------------------------------------------------------------------

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

