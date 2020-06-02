module Util where

import           System.Environment
import           System.Exit

---- userError -----------------------------------------------

exitWithMessage :: String -> IO a
exitWithMessage msg = do
                nm <- getProgName
                putStrLn (nm ++ ": " ++ msg)
                exitFailure

---- split,unsplit ------------------------------------------------------------

-- split,unsplit a generalization of words,unwords! (but not too general :-)

split        :: Eq a => a -> [a] -> [[a]]
split x = splitBy (==x)

splitBy      :: (a->Bool) -> [a] -> [[a]]
splitBy _ []  = []
splitBy p xs  = let (l,xs') = break p xs
                in l : case xs' of []       -> []
                                   (_:xs'') -> splitBy p xs''

unsplit :: a -> [[a]] -> [a]
unsplit _ [] = []
unsplit x xs = foldr1 (\x' s -> x' ++ x:s) xs

unsplit' :: a -> [[a]] -> [a]
unsplit' x xs = foldr (\x' s -> x' ++ x:s) [] xs
