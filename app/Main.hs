module Main where

-- import           Lib
import           System.Console.Haskeline
-- import           System.Environment
import           System.IO
import           Text.Parsec
import           Text.ParserCombinators.Parsec
-- import           Text.Pretty.Simple

data Sexy = NonNorm  (SexyFunc, Sexy, Sexy)
          | Norm     Value

data SexyFunc = Arthmetic (String, (Sexy -> Sexy -> Sexy)) -- (Sexy -> Sexy -> Sexy) -- (Integer -> Integer -> Integer)
              | UnknownFunc String

data Value = SexyInteger Integer
           | SexyError   String
           -- | SexyBool    Bool

sexyAdd :: Sexy -> Sexy -> Sexy
sexyAdd (Norm (SexyInteger val1)) (Norm (SexyInteger val2)) =
  (Norm . SexyInteger) (val1 + val2)
sexyAdd val1 val2 =
  (Norm . SexyError) $ "(Error: Failed at addition of " ++ show val1 ++ " and " ++ show val2 ++ ".)"

sexySub :: Sexy -> Sexy -> Sexy
sexySub (Norm (SexyInteger arg0)) (Norm (SexyInteger arg1)) =
  (Norm . SexyInteger) (arg0 - arg1)
sexySub arg0 arg1 =
  (Norm . SexyError) $ "(Error: Failed at subtraction of " ++ show arg0 ++ " by " ++ show arg1 ++ ".)"

sexyMulti :: Sexy -> Sexy -> Sexy
sexyMulti (Norm (SexyInteger arg0)) (Norm (SexyInteger arg1)) =
  (Norm . SexyInteger) (arg0 * arg1)
sexyMulti arg0 arg1 =
  (Norm . SexyError) $ "(Error: Failed at muliplication of " ++ show arg0 ++ " and " ++ show arg1 ++ ".)"

sexyDiv :: Sexy -> Sexy -> Sexy
sexyDiv (Norm (SexyInteger arg0)) (Norm (SexyInteger arg1)) =
  (Norm . SexyInteger) (div arg0 arg1)
sexyDiv arg0 arg1 =
  (Norm . SexyError) $ "(Error: Failed at division of " ++ show arg0 ++ " by " ++ show arg1 ++ ".)"

sexyMod :: Sexy -> Sexy -> Sexy
sexyMod (Norm (SexyInteger arg0)) (Norm (SexyInteger arg1)) =
  (Norm . SexyInteger) (mod arg0 arg1)
sexyMod arg0 arg1 =
  (Norm . SexyError) $ "(Error: Failed to yield the remainder from division of " ++ show arg0 ++ " by " ++ show arg1 ++ ".)"

sexyUnkownFunc :: Sexy -> Sexy -> Sexy
sexyUnkownFunc arg0 arg1 =
  (Norm . SexyError) $ "(Error: Unknown function applied to " ++ show arg0 ++ " and " ++ show arg1 ++ ".)"

showSexyFunc :: SexyFunc -> String
showSexyFunc (Arthmetic ("+", _)) = "(Arthmetic: addition)"
showSexyFunc (Arthmetic ("-", _)) = "(Arthmetic: subtraction)"
showSexyFunc (Arthmetic ("*", _)) = "(Arthmetic: mutiplication)"
showSexyFunc (Arthmetic ("/", _)) = "(Arthmetic: division)"
showSexyFunc (Arthmetic ("%", _)) = "(Arthmetic: modulo)"
showSexyFunc (Arthmetic (val, _)) = "(Unkown Arthmetic: " ++ val ++ ")"
showSexyFunc (UnknownFunc func)   = "(Unknown function: " ++ func ++ ")"

instance Show SexyFunc where show = showSexyFunc

showSexy :: Sexy -> String
showSexy (NonNorm (sexyfunc, sexy1, sexy2)) =
  showSexyFunc sexyfunc ++ " (Sexy: " ++ showSexy sexy1 ++ ") (Sexy: " ++ showSexy sexy2 ++ ")"
showSexy (Norm types) = showValue types

instance Show Sexy where show = showSexy

showValue :: Value -> String
showValue (SexyInteger integer) = "(SexyInteger: " ++ show integer ++ ")"
showValue (SexyError err)       = "(SexyError: " ++ err ++ ")"

instance Show Value where show = showValue

parseSexy :: Parser Sexy
parseSexy = -- spaces >> (node <|> parsed)
  spaces >> (parseSexyNonNorm <|> parseSexyNorm)

parseSexyNonNorm :: Parser Sexy
parseSexyNonNorm = do
  _ <- char '('
  funcKey <- spaces >> manyTill anyToken (Text.ParserCombinators.Parsec.try space)
  sexy1:_ <- spaces >> manyTill parseSexy
    ((Text.ParserCombinators.Parsec.try space) <|> (Text.ParserCombinators.Parsec.try (char ')')))
  sexy2:[] <- spaces >> manyTill parseSexy
    ((Text.ParserCombinators.Parsec.try space) <|> (char ')'))

  let sexyFunc = case lookup funcKey sexyFuncs of
                  Just funcKeyhole -> (funcKey, funcKeyhole)
                  Nothing          -> (funcKey, sexyUnkownFunc)
  return $ NonNorm (Arthmetic sexyFunc, sexy1, sexy2)

parseSexyNorm :: Parser Sexy
parseSexyNorm = do
  atom <- many1 (digit <|> letter)

  return $ Norm (SexyInteger (read atom))

sexyFuncs :: [(String, Sexy -> Sexy -> Sexy)]
sexyFuncs = [("+", sexyAdd),
             ("-", sexySub),
             ("*", sexyMulti),
             ("/", sexyDiv),
             ("%", sexyMod)]

evalSexyFunc :: SexyFunc -> (Sexy -> Sexy -> Sexy)
evalSexyFunc (Arthmetic (_, sexyFunc)) = sexyFunc
evalSexyFunc (UnknownFunc _)           = sexyUnkownFunc

evalSexy :: Sexy -> Sexy
evalSexy sexy =
  case sexy of
    NonNorm (sexyFunc, sexy1, sexy2) -> (evalSexyFunc sexyFunc) (evalSexy sexy1) (evalSexy sexy2)
    val@(Norm (SexyInteger _))       -> val
    val@(Norm (SexyError _))         -> val


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

repl :: InputT IO ()
repl = do
  maybeInput <- getInputLine "(SexyEval: )>>> "
  let input = case maybeInput of
       Just theInput -> theInput
       Nothing       -> "Error: failed getting input" in
    outputStrLn $ show
    $ evalSexy
    $ case (parse parseSexy "" input) of
    Right sexy -> sexy
    Left err   -> (Norm . SexyError) (show err)
  repl



main :: IO ()
main = do
  runInputT defaultSettings repl
--           args <- getArgs
--           putStrLn $ head args
--           pPrint (parse parseSexy "" (head args))
--           print
--             $ evalSexy
--             $ case (parse parseSexy "" (head args)) of
--                 Right sexy -> sexy
--                 Left err   -> Norm (TypeError (show err))
--           -- print (listToken (head args))
