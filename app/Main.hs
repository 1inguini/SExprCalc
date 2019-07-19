module Main where

-- import           Lib
-- import           Text.Pretty.Simple
-- import           System.Environment
-- import qualified System.IO                     as IO
import qualified Data.Either                   as E
import qualified Data.Maybe                    as M
import qualified System.Console.Haskeline      as HLine
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as PCP


data Sexy = NonNorm  (SexyFunc, Sexy, Sexy)
          | Norm     Value

data SexyFunc = Arthmetic (String, (Sexy -> Sexy -> Sexy)) -- (Sexy -> Sexy -> Sexy) -- (Integer -> Integer -> Integer)
              | UnknownFunc String

data Value = SexyInteger Integer
           | SexyError   String
           -- | SexyBool    Bool

sexyFuncs :: [(String, Sexy -> Sexy -> Sexy)]
sexyFuncs = [("+", sexyAdd),
             ("-", sexySub),
             ("*", sexyMulti),
             ("/", sexyDiv),
             ("%", sexyMod)]

sexyAdd :: Sexy -> Sexy -> Sexy
sexyAdd (Norm (SexyInteger val1)) (Norm (SexyInteger val2)) =
  (Norm . SexyInteger) (val1 + val2)
sexyAdd val1 val2 =
  (Norm . SexyError) $ "(Error: failed at addition of " ++ show val1 ++ " and " ++ show val2 ++ ")"

sexySub :: Sexy -> Sexy -> Sexy
sexySub (Norm (SexyInteger arg0)) (Norm (SexyInteger arg1)) =
  (Norm . SexyInteger) (arg0 - arg1)
sexySub arg0 arg1 =
  (Norm . SexyError) $ "(Error: failed at subtraction of " ++ show arg0 ++ " by " ++ show arg1 ++ ")"

sexyMulti :: Sexy -> Sexy -> Sexy
sexyMulti (Norm (SexyInteger arg0)) (Norm (SexyInteger arg1)) =
  (Norm . SexyInteger) (arg0 * arg1)
sexyMulti arg0 arg1 =
  (Norm . SexyError) $ "(Error: failed at muliplication of " ++ show arg0 ++ " and " ++ show arg1 ++ ")"

sexyDiv :: Sexy -> Sexy -> Sexy
sexyDiv (Norm (SexyInteger arg0)) (Norm (SexyInteger arg1)) =
  (Norm . SexyInteger) (div arg0 arg1)
sexyDiv arg0 arg1 =
  (Norm . SexyError) $ "(Error: failed at division of " ++ show arg0 ++ " by " ++ show arg1 ++ ")"

sexyMod :: Sexy -> Sexy -> Sexy
sexyMod (Norm (SexyInteger arg0)) (Norm (SexyInteger arg1)) =
  (Norm . SexyInteger) (mod arg0 arg1)
sexyMod arg0 arg1 =
  (Norm . SexyError) $ "(Error: failed to yield the remainder from division of " ++ show arg0 ++ " by " ++ show arg1 ++ ")"

sexyUnkownFunc :: Sexy -> Sexy -> Sexy
sexyUnkownFunc arg0 arg1 =
  (Norm . SexyError) $ "(Error: Unknown function applied to " ++ show arg0 ++ " and " ++ show arg1 ++ ")"

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

parseSexy :: PCP.Parser Sexy
parseSexy = -- spaces >> (node <|> parsed)
  PCP.spaces >> (parseSexyNonNorm PCP.<|> parseSexyNorm)

parseSexyNonNorm :: PCP.Parser Sexy
parseSexyNonNorm = do
  _ <- PCP.char '('
  funcKey <- PCP.spaces >> PCP.manyTill PCP.anyToken (PCP.try PCP.space)
  sexy1:_ <- PCP.spaces >> PCP.manyTill parseSexy
    ((PCP.try PCP.space) PCP.<|> (PCP.try (PCP.char ')')))
  sexy2:[] <- PCP.spaces >> PCP.manyTill parseSexy
    ((PCP.try PCP.space) PCP.<|> (PCP.char ')'))

  let sexyFunc = case lookup funcKey sexyFuncs of
                  Just funcKeyhole -> (funcKey, funcKeyhole)
                  Nothing          -> (funcKey, sexyUnkownFunc)
    in
    return $ NonNorm (Arthmetic sexyFunc, sexy1, sexy2)

parseSexyNorm :: PCP.Parser Sexy
parseSexyNorm = do
  atom <- PCP.many1 (PCP.digit PCP.<|> PCP.letter)

  return $ Norm (SexyInteger (read atom))

evalSexyFunc :: SexyFunc -> (Sexy -> Sexy -> Sexy)
evalSexyFunc (Arthmetic (_, sexyFunc)) = sexyFunc
evalSexyFunc (UnknownFunc _)           = sexyUnkownFunc

evalSexy :: Sexy -> Sexy
evalSexy sexy =
  case sexy of
    NonNorm (sexyFunc, sexy1, sexy2) -> (evalSexyFunc sexyFunc) (evalSexy sexy1) (evalSexy sexy2)
    val@(Norm (SexyInteger _))       -> val
    val@(Norm (SexyError _))         -> val


-- flushStr :: String -> IO ()
-- flushStr str = putStr str >> IO.hFlush IO.stdout

repl :: HLine.InputT IO ()
repl = do
  maybeInput <- HLine.getInputLine "(SexyEval: )>>> "
  let input = "Error: failed getting input" `M.fromMaybe` maybeInput
       -- case maybeInput of
       -- Just theInput -> theInput
       -- Nothing       -> "Error: failed getting input"
    in
    HLine.outputStrLn $ show
    $ evalSexy
    $ E.either (Norm . SexyError . show) id $ P.parse parseSexy "" input
      -- case (parse parseSexy "" input) of
      --   Right sexy -> sexy
      --   Left err   -> (Norm . SexyError) (show err)
  repl

main :: IO ()
main = do
  HLine.runInputT HLine.defaultSettings repl
--           args <- getArgs
--           putStrLn $ head args
--           pPrint (parse parseSexy "" (head args))
--           print
--             $ evalSexy
--             $ case (parse parseSexy "" (head args)) of
--                 Right sexy -> sexy
--                 Left err   -> Norm (TypeError (show err))
--           -- print (listToken (head args))
