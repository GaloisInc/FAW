module Util where

import           Control.Monad
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO.Error

---- userError -----------------------------------------------

exitWithMessage :: String -> IO a
exitWithMessage msg = do
                nm <- getProgName
                putStrLn (nm ++ ": " ++ msg)
                exitFailure

---- file operations ---------------------------------------------------------

-- | "Tries" to remove a file.  If file does not exist, ignore.
tryRemoveFile :: FilePath -> IO ()
tryRemoveFile path = catchIOError (removeFile path) $
    \e -> unless (isDoesNotExistError e) $ ioError e


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
