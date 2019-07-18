module Main where

-- import           Lib
import           System.Environment
import           Text.Parsec
import           Text.ParserCombinators.Parsec
import           Text.Pretty.Simple

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
