module Main where

-- import           Lib
import           Control.Monad
import           Data.List
import           System.Environment
import           Text.Parsec
import           Text.ParserCombinators.Parsec
import           Text.Pretty.Simple

listToString :: String -> [String] -> String
listToString seperator =
  foldl1 (\theWords word -> theWords ++  seperator ++ word)

data SVal = Expr    Expr
          | Integer Integer
          | Bool    Bool
          | List    Tree
          | SValError String


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

data Expr = Add
          | Sub
          | Times
          | Div
          | Mod

data Tree = Node [Tree]
          | Leaf SVal

instance Show SVal where show = showSVal
instance Show Tree where show = showTree

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

data Sexy = NonNorm  (SexyFunc, Sexy, Sexy)
          | Norm     Types

data SexyFunc = Arthmetic String -- (Integer -> Integer -> Integer)

data Types = SexyInteger Integer
           -- | SexyBool    Bool

showSexyFunc :: SexyFunc -> String
showSexyFunc (Arthmetic "+") = "(Function: addition)"
showSexyFunc (Arthmetic "-") = "(Function: subtraction)"
showSexyFunc (Arthmetic "*") = "(Function: mutiplication)"
showSexyFunc (Arthmetic "/") = "(Function: division)"
showSexyFunc (Arthmetic "%") = "(Function: modulo)"
showSexyFunc (Arthmetic val) = "(Error: unknown function " ++ val ++ " )"
instance Show SexyFunc where show = showSexyFunc

showSexy :: Sexy -> String
showSexy (NonNorm (sexyfunc, sexy1, sexy2)) =
  showSexyFunc sexyfunc ++ " (Sexy: " ++ showSexy sexy1 ++ ") (Sexy: " ++ showSexy sexy2 ++ ")"
showSexy (Norm types) = showTypes types
instance Show Sexy where show = showSexy

showTypes :: Types -> String
showTypes (SexyInteger integer) = "(SexyInteger: " ++ show integer ++ ")"
instance Show Types where show = showTypes

parseSexy :: Parser Sexy
parseSexy = -- spaces >> (node <|> parsed)
  spaces >> (parseSexyNonNorm <|> parseSexyNorm)

parseSexyNorm :: Parser Sexy
parseSexyNorm = do
  atom <- many1 (digit <|> letter)
  return $ Norm (SexyInteger (read atom))

parseSexyNonNorm :: Parser Sexy
parseSexyNonNorm = do
  _ <- char '('
  func  <- spaces >> manyTill anyToken (Text.ParserCombinators.Parsec.try space)
  sexy1:_ <- spaces >> manyTill parseSexy
    ((Text.ParserCombinators.Parsec.try space) <|> (Text.ParserCombinators.Parsec.try (char ')')))
  sexy2:[] <- spaces >> manyTill parseSexy
    ((Text.ParserCombinators.Parsec.try space) <|> (char ')'))
  return $ NonNorm (Arthmetic func, sexy1, sexy2)

main :: IO ()
main = do
          args <- getArgs
          print $ head args
          pPrint (parse parseSexy "" (head args))
          parseTest parseSexy (head args)
          -- print (listToken (head args))
