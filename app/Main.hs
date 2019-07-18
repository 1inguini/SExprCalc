module Main where

-- import           Lib
import           System.Environment
import           System.IO
import           Text.Parsec
import           Text.ParserCombinators.Parsec
import           Text.Pretty.Simple

data Sexy = NonNorm  (SexyFunc, Sexy, Sexy)
          | Norm     Types

data SexyFunc = Arthmetic String -- (Sexy -> Sexy -> Sexy) -- (Integer -> Integer -> Integer)
              | UnknownFunc

data Types = SexyInteger Integer
           | UnknownType
           | TypeError   String
           -- | SexyBool    Bool

sexyAdd :: Sexy -> Sexy -> Sexy
sexyAdd (Norm (SexyInteger val1)) (Norm (SexyInteger val2)) =
  (Norm . SexyInteger) (val1 + val2)
sexyAdd _ _ = Norm UnknownType

sexySub :: Sexy -> Sexy -> Sexy
sexySub (Norm (SexyInteger val1)) (Norm (SexyInteger val2)) =
  (Norm . SexyInteger) (val1 - val2)
sexySub _ _ = Norm UnknownType

sexyMulti :: Sexy -> Sexy -> Sexy
sexyMulti (Norm (SexyInteger val1)) (Norm (SexyInteger val2)) =
  (Norm . SexyInteger) (val1 * val2)
sexyMulti _ _ = Norm UnknownType

sexyDiv :: Sexy -> Sexy -> Sexy
sexyDiv (Norm (SexyInteger val1)) (Norm (SexyInteger val2)) =
  (Norm . SexyInteger) (div val1 val2)
sexyDiv _ _ = Norm UnknownType

sexyMod :: Sexy -> Sexy -> Sexy
sexyMod (Norm (SexyInteger val1)) (Norm (SexyInteger val2)) =
  (Norm . SexyInteger) (mod val1 val2)
sexyMod _ _ = Norm UnknownType

sexyUnkownFunc :: Sexy -> Sexy -> Sexy
sexyUnkownFunc _ _ = Norm UnknownType

showSexyFunc :: SexyFunc -> String
showSexyFunc (Arthmetic "+") = "(Function: addition)"
showSexyFunc (Arthmetic "-") = "(Function: subtraction)"
showSexyFunc (Arthmetic "*") = "(Function: mutiplication)"
showSexyFunc (Arthmetic "/") = "(Function: division)"
showSexyFunc (Arthmetic "%") = "(Function: modulo)"
showSexyFunc UnknownFunc     = "(Error: unknown)"
showSexyFunc _               = "(Error: unknown function)"

instance Show SexyFunc where show = showSexyFunc

showSexy :: Sexy -> String
showSexy (NonNorm (sexyfunc, sexy1, sexy2)) =
  showSexyFunc sexyfunc ++ " (Sexy: " ++ showSexy sexy1 ++ ") (Sexy: " ++ showSexy sexy2 ++ ")"
showSexy (Norm types) = showTypes types

instance Show Sexy where show = showSexy

showTypes :: Types -> String
showTypes (SexyInteger integer) = "(SexyInteger: " ++ show integer ++ ")"
showTypes UnknownType           = "(UnknownType)"
showTypes (TypeError err)       = "(TypeError:" ++ err ++ ")"

instance Show Types where show = showTypes

parseSexy :: Parser Sexy
parseSexy = -- spaces >> (node <|> parsed)
  spaces >> (parseSexyNonNorm <|> parseSexyNorm)

parseSexyNonNorm :: Parser Sexy
parseSexyNonNorm = do
  _ <- char '('
  func  <- spaces >> manyTill anyToken (Text.ParserCombinators.Parsec.try space)
  sexy1:_ <- spaces >> manyTill parseSexy
    ((Text.ParserCombinators.Parsec.try space) <|> (Text.ParserCombinators.Parsec.try (char ')')))
  sexy2:[] <- spaces >> manyTill parseSexy
    ((Text.ParserCombinators.Parsec.try space) <|> (char ')'))
  return $ NonNorm (Arthmetic func, sexy1, sexy2)

-- matchSexyFunc :: String -> SexyFunc
-- matchSexyFunc "+" = (Arthmetic sexyAdd)
-- matchSexyFunc "-" = (Arthmetic sexySub)
-- matchSexyFunc "*" = (Arthmetic sexyMulti)
-- matchSexyFunc "/" = (Arthmetic sexyDiv)
-- matchSexyFunc "%" = (Arthmetic sexyMod)
-- matchSexyFunc _   =  UnknownFunc

parseSexyNorm :: Parser Sexy
parseSexyNorm = do
  atom <- many1 (digit <|> letter)
  return $ Norm (SexyInteger (read atom))


evalSexyFunc :: SexyFunc -> (Sexy -> Sexy -> Sexy)
evalSexyFunc (Arthmetic "+") = sexyAdd
evalSexyFunc (Arthmetic "-") = sexySub
evalSexyFunc (Arthmetic "*") = sexyMulti
evalSexyFunc (Arthmetic "/") = sexyDiv
evalSexyFunc (Arthmetic "%") = sexyMod
evalSexyFunc _               = sexyUnkownFunc

evalSexy :: Sexy -> Sexy
evalSexy sexy =
  case sexy of
    NonNorm (sexyFunc, sexy1, sexy2) -> (evalSexyFunc sexyFunc) (evalSexy sexy1) (evalSexy sexy2)
    Norm  sexyInteger                -> Norm  sexyInteger


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

repl :: IO ()
repl = do
  flushStr "SExprCalc >>> "
  input <- getLine
  flushStr $ show
    $ evalSexy
    $ case (parse parseSexy "" input) of
    Right sexy -> sexy
    Left err   -> Norm (TypeError (show err))
  flushStr "\n"
  repl

main :: IO ()
main = do
--           args <- getArgs
--           putStrLn $ head args
--           pPrint (parse parseSexy "" (head args))
--           print
--             $ evalSexy
--             $ case (parse parseSexy "" (head args)) of
--                 Right sexy -> sexy
--                 Left err   -> Norm (TypeError (show err))
--           -- print (listToken (head args))
  repl
