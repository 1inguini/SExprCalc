module Main where

-- import           Lib
import           Control.Monad
import           Data.List
import           System.Environment
import           Text.Parsec
import           Text.ParserCombinators.Parsec

listToString :: String -> [String] -> String
listToString seperator =
  foldl1 (\theWords word -> theWords ++  seperator ++ word)

data SVal = Expr    Expr
          | Integer Integer
          | Bool    Bool
          | List    Tree
          | SValError String

data Expr = Add
          | Sub
          | Times
          | Div
          | Mod

data Tree = Node [Tree]
          | Leaf SVal

showTree :: Tree -> String
showTree (Leaf sval)  = showSVal sval
showTree (Node trees) =
  "< " ++ listToString " " (map showTree trees) ++ " >"

parseTree :: Parser Tree
parseTree = spaces >> (node <|> parsed)
  where
    node = Node <$> between (char '(') (char ')') (many1 parseTree)
    leaf = many1 (oneOf "+-*/%" <|> digit) <|> sepBy1 space (many (noneOf "()"))
    parsed = Leaf . sVal <$> leaf

sVal :: String -> SVal
sVal "+" = Expr Add
sVal "-" = Expr Sub
sVal "*" = Expr Times
sVal "/" = Expr Div
sVal "%" = Expr Mod
sVal "t" = Bool True
sVal "f" = Bool False
sVal str = case parse (many1 digit) "" str of
  Right num -> Integer (read num)
  _         -> SValError ("'" ++ str ++ "' is not valid character")

showSVal :: SVal -> String
showSVal (Expr Add)        = "add"
showSVal (Expr Sub)        = "sub"
showSVal (Expr Times)      = "times"
showSVal (Expr Div)        = "div"
showSVal (Expr Mod)        = "mod"
showSVal (Integer integer) = show integer
showSVal (Bool True)       = "true"
showSVal (Bool False)      = "false"
showSVal (List tree)       = show tree
showSVal (SValError str)   = "SValError: " ++ (init . tail) (show str)

instance Show SVal where show = showSVal
instance Show Tree where show = showTree

main :: IO ()
main = do
          args <- getArgs
          print $ head args
          print (parse parseTree "" (head args))
          -- print (listToken (head args))
