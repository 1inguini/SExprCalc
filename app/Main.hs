module Main where

-- import           Lib
-- import           System.Environment
-- import qualified System.IO                     as IO
-- import qualified Safe                          as Safe
-- import qualified Data.Maybe                    as M
import qualified Data.Either                   as E
import qualified Data.Text.Lazy                as TLazy
import qualified System.Console.Haskeline      as HLine
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as PCP
import           Text.Pretty.Simple            as PrettyS

data Sexy = NonNorm  (SexyFunc, Sexy, Sexy)
          | Norm     Value

data SexyFunc = Arthmetic (String, (Sexy -> Sexy -> Sexy))
              | UnknownFunc String

data Value = SexyInteger Integer
           | SexyError   String
           | EOF
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
showValue EOF                   = "(EOF: end of file)"

instance Show Value where show = showValue

parseSexy :: PCP.Parser Sexy
parseSexy = do -- spaces >> (node <|> parsed)
  do {PCP.eof; return $ Norm EOF} <|> (PCP.spaces >> (parseSexyNonNorm <|> parseSexyNorm))

parseSexyNonNorm :: PCP.Parser Sexy
parseSexyNonNorm = do
  _ <- PCP.char '('
  funcKey <- PCP.spaces >> PCP.manyTill PCP.anyToken (PCP.try PCP.space)
  sexy0:sexy1:empty <- PCP.spaces >> (PCP.between (PCP.spaces) (PCP.spaces) (parseSexy)
                                      <|> (PCP.spaces >> parseSexy)) `PCP.manyTill` (PCP.char ')')
  let
    sexyFunc = case lookup funcKey sexyFuncs of
                  Just funcKeyhole -> (funcKey, funcKeyhole)
                  Nothing          -> (funcKey, sexyUnkownFunc)
    -- arg0 = case PCP.parse parseSexy "" sexy0 of
    --          Right val -> val
    --          Left  val -> (Norm . SexyError . show) val
    -- arg1 = case PCP.parse parseSexy "" sexy1 of
    --          Right val -> val
    --          Left  val -> (Norm . SexyError . show) val
    in
    return $ if null empty
             then NonNorm (Arthmetic sexyFunc, sexy0, sexy1)
             else (Norm . SexyError)
                  $ "(Error: " ++ show (Arthmetic sexyFunc) ++ " took arguments other than " ++ show sexy0 ++ " " ++ show sexy1 ++ ", which are " ++ (foldl1 (\xs x -> xs ++ " " ++ x) (map show empty))

parseSexyNorm :: PCP.Parser Sexy
parseSexyNorm =
  do {(Norm . SexyInteger . read) <$> PCP.try (PCP.many1 PCP.digit)}
  <|>
  do {(Norm . SexyError . \string -> string ++ " is not an integer") <$> PCP.try (PCP.many1 PCP.letter)}

evalSexyFunc :: SexyFunc -> (Sexy -> Sexy -> Sexy)
evalSexyFunc (Arthmetic (_, sexyFunc)) = sexyFunc
evalSexyFunc (UnknownFunc _)           = sexyUnkownFunc

evalSexy :: Sexy -> Sexy
evalSexy sexy =
  case sexy of
    NonNorm (sexyFunc, sexy1, sexy2) -> (evalSexyFunc sexyFunc) (evalSexy sexy1) (evalSexy sexy2)
    val@(Norm (SexyInteger _))       -> val
    val@(Norm (SexyError _))         -> val
    val@(Norm EOF)                   -> val


-- flushStr :: String -> IO ()
-- flushStr str = putStr str >> IO.hFlush IO.stdout

rep :: Maybe String -> HLine.InputT IO ()
rep (Just input) = do
    (HLine.outputStrLn . TLazy.unpack . PrettyS.pShow)
      $ evalSexy
      $ E.either (Norm . SexyError . show) id $ P.parse parseSexy "" input
      -- case (parse parseSexy "" input) of
      --   Right sexy -> sexy
      --   Left err   -> (Norm . SexyError) (show err)
    repl
rep Nothing = do
  HLine.outputStrLn . TLazy.unpack . PrettyS.pString $ "(SexyFarewell: Bye!)"
  return ()

repl :: HLine.InputT IO ()
repl = do
  maybeInput <- HLine.getInputLine "(SexyEval: )>>> "
  rep maybeInput

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
