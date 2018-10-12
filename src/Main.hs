module Main where
  import Data.Char
  import Data.Maybe
  import Control.Monad.Loops

  toDigit :: Char -> Maybe String
  toDigit c
    | isDigit c = Just $ [":zero:", ":one:", ":two:", ":three:", ":four:", 
                          ":five:", ":six:", ":seven:", ":eight:", ":nine:", 
                          ":ten:"] !! (digitToInt c)
    | otherwise = Nothing

  toYell :: Char -> String
  toYell c
    | isAlpha c = ":regional_indicator_" ++ [toLower c] ++ ": "
    | isDigit c = (++ " ") . fromJust $ toDigit c
    | c == '!'  = ":exclamation: "
    | c == '?'  = ":question: "
    | c == '.'  = ":black_small_square: "
    | otherwise = [c]


  main :: IO ()
  main = do
    putStrLn "Enter your text here to YELL it:"
    whileM_ (return True) (getLine >>= (putStrLn . concat . (fmap toYell)))
    return ()
