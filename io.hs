{-main :: IO ()
main = do 
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Hey " <> name <> ", you rock!")
-}

{-import qualified Data.List as List
main :: IO ()
main = do
    line <- getLine
    if null line
        then pure ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords string = unwords $ map reverse $ words string
-}

{-main :: IO ()
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
-}
import qualified Data.Char as Char

main :: IO ()
main = do
    contents <- getContents
    putStr (map Char.toUpper contents)