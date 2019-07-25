module Main where

-- import           Lib
-- import qualified Data.IORef                    as IORef
-- import qualified System.IO                     as IO
-- import qualified Data.List                     as List
import qualified Data.Either                   as E
import           Data.Map.Strict               ((!?))
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as May
import qualified Data.Text.Lazy                as TLazy
import qualified Safe                          as Safe
import qualified System.Console.Haskeline      as HLine
import qualified System.Environment            as Env
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as PCP
import qualified Text.Pretty.Simple            as PrettyS

type SexyKey = String

type SexyEnv = (Map.Map SexyKey Sexy)

data Sexy = NonNorm  (Sexy, Sexy, Sexy)
          | LazyNonNorm (Sexy, Sexy, Sexy)
          | SexyFunction (SexyEnv -> Sexy -> Sexy -> Sexy)
          | SexyAtom     SexyKey
          | SexyInteger  Integer
          | SexyText     TLazy.Text
          | SexyBool     Bool
          | SexyCouple   (Sexy, Sexy)
          | SexyVoid
          | SexyError String
          | EOF

sexyFuncs :: Map.Map SexyKey Sexy
sexyFuncs = Map.fromList
  $ [ ("+", SexyFunction sexyAdd)
    , ("-", SexyFunction sexySub)
    , ("*", SexyFunction sexyMulti)
    , ("/", SexyFunction sexyDiv)
    , ("%", SexyFunction sexyMod)
    , ("if", SexyFunction sexyIf)
    , ("?", SexyFunction sexyIsSuccess)
    , ("eval", SexyFunction sexyEvalText)
    , ("couple", SexyFunction sexyCouple)
    , (".", SexyFunction sexyCouple)
    , ("do", SexyFunction sexyDo)
    , ("let", SexyFunction sexyLet)
    , ("T", SexyBool True)
    , ("F", SexyBool False)
    ]

symbol :: PCP.Parser Char
symbol = PCP.oneOf ".!#$%&|*+-/:<=>?@^~"

parseSexy, parseNonNorm, parseLazyNonNorm, parseAtom, parseInteger, parseText, parseVoid
  :: PCP.Parser Sexy

parseSexy = PCP.spaces >>
  PCP.choice [ PCP.try parseNonNorm
             , PCP.try parseLazyNonNorm
             , PCP.try parseAtom
             , PCP.try parseInteger
             , PCP.try parseText
             , PCP.try parseVoid ]

parseNonNorm = do
  _       <- PCP.spaces >> PCP.char '('
  command <- PCP.spaces >> parseAtom
  arg0    <- PCP.spaces >> parseSexy
  arg1    <- PCP.spaces >> parseSexy
  _       <- PCP.spaces >> PCP.char ')'
  return $ NonNorm (command, arg0, arg1)

parseLazyNonNorm = do
  _       <- PCP.spaces >> PCP.char '{'
  command <- PCP.spaces >> parseAtom
  arg0    <- PCP.spaces >> parseSexy
  arg1    <- PCP.spaces >> parseSexy
  _       <- PCP.spaces >> PCP.char '}'
  return $ LazyNonNorm (command, arg0, arg1)

parseAtom = do
    x <- P.try PCP.letter <|> symbol
    xs <- PCP.many $ PCP.try PCP.letter <|> PCP.try PCP.digit <|> symbol
    return $ SexyAtom $ x:xs

parseInteger = do
  -- x  <- P.try (PCP.char '+') <|> PCP.char '-'
  xs <- PCP.many1 $ PCP.try PCP.digit
  return $ SexyInteger . read $ xs

parseText = do
  SexyText . TLazy.pack
    <$> PCP.between (PCP.char '"') (PCP.char '"')
    (PCP.many (PCP.try parseEscape
               <|> PCP.noneOf "\""))
  where
    parseEscape :: PCP.Parser Char
    parseEscape = do
      _ <- PCP.char '\\'
      PCP.anyToken

parseVoid = do
  _ <- PCP.char '_'
  return SexyVoid

sexyAdd, sexySub, sexyMulti, sexyDiv, sexyMod, sexyEvalText, sexyIf, sexyIsSuccess ,sexyLet, sexyDo, sexyCouple
  :: SexyEnv -> Sexy -> Sexy -> Sexy

sexyAdd _ (SexyInteger val1) (SexyInteger val2) =
  SexyInteger $ val1 + val2
sexyAdd e arg0 arg1 =
  SexyError $ "(SexyEnv: " ++ show e ++ "), failed at addition of " ++ show arg0 ++ " and " ++ show arg1

sexySub _ (SexyInteger arg0) (SexyInteger arg1) =
  SexyInteger $ arg0 - arg1
sexySub e arg0 arg1 =
  SexyError $ "(SexyEnv: " ++ show e ++ "), failed at subtraction of " ++ show arg0 ++ " by " ++ show arg1

sexyMulti _ (SexyInteger arg0) (SexyInteger arg1) =
  SexyInteger $ arg0 * arg1
sexyMulti e arg0 arg1 =
  SexyError $ "(SexyEnv: " ++ show e ++ "), failed at muliplication of " ++ show arg0 ++ " and " ++ show arg1

sexyDiv _ (SexyInteger arg0) (SexyInteger arg1) =
  SexyInteger $ div arg0 arg1
sexyDiv e arg0 arg1 =
  SexyError $ "(SexyEnv: " ++ show e ++ "), failed at division of " ++ show arg0 ++ " by " ++ show arg1

sexyMod _ (SexyInteger arg0) (SexyInteger arg1) =
  SexyInteger $ mod arg0 arg1
sexyMod e arg0 arg1 =
  SexyError $ "(SexyEnv: " ++ show e ++ "), failed to yield the remainder from division of " ++ show arg0 ++ " by " ++ show arg1

sexyIf e (SexyBool bool) (SexyCouple (thethen, theelse)) =
  if bool then ((evalSexy e thethen)) else (evalSexy e theelse)
sexyIf e arg0@(SexyBool _) arg1 =
  SexyError $ "(SexyEnv: " ++ show e ++ "), the first arg was " ++ show arg0 ++ " but the second argument were not SexyCouple but " ++ show arg1
sexyIf e arg0 arg1 =
  SexyError $ "(SexyEnv: " ++ show e ++ "), the first arg was not SexyBool but " ++ show arg0 ++ " and the second argument were " ++ show arg1

sexyIsSuccess _ SexyVoid sexy =
  case sexy of
      SexyError _ -> SexyBool False
      _           -> SexyBool True
sexyIsSuccess e arg0 arg1 =
  SexyError $ "(SexyEnv: " ++ show e ++ "), the first argument is not SexyVoid but " ++ show arg0 ++ "; the second argument is " ++ show arg1

sexyEvalText e SexyVoid (SexyText code) =
  (evalSexy e $ E.either (SexyError . show) id $ P.parse parseSexy "" (TLazy.unpack code))
sexyEvalText e SexyVoid arg1 =
  SexyError
    $ "(SexyEnv: " ++ show e ++ "), the first argument was SexyVoid but the second argument was not SexyText but " ++ show arg1
sexyEvalText e arg0 arg1 =
  SexyError
    $ "(SexyEnv: " ++ show e ++ "), the first argument is not SexyVoid but " ++ show arg0 ++ "; the second argument is " ++ show arg1

setSexyVar :: SexyEnv -> SexyKey -> Sexy -> SexyEnv
setSexyVar e sexyKey sexyToBeBounded =
  Map.insert sexyKey sexyToBeBounded e

defineSexyVar :: SexyEnv -> SexyKey -> Sexy -> Either Sexy SexyEnv
defineSexyVar e sexyKey sexyToBeBounded =
  May.maybe
  (Right $ Map.insert sexyKey sexyToBeBounded e)
  (\x -> Left . SexyError $ show x ++ " already binded")
  (e !? sexyKey)

sexyDo e arg0@(NonNorm _) arg1@(NonNorm _) =
  case (evalSexy e arg0) of
    sexy@(SexyError _) ->
      SexyError $ "failed doing "
        ++ show arg0 ++ " with error " ++ show sexy
        ++ " before doing " ++ show arg1
    _ -> (evalSexy e arg1)
sexyDo e arg0 arg1 =
  SexyError
    $ "(SexyEnv: " ++ show e ++ "), failed at doing " ++ show arg0 ++ " and " ++ show arg1

sexyCouple e (SexyError err) sexy1 =
  SexyError $ "(SexyEnv: " ++ show e ++ "), can't make couple with " ++ err ++ " and " ++ show sexy1
sexyCouple e sexy0 (SexyError err) =
  SexyError $ "(SexyEnv: " ++ show e ++ "), can't make couple with " ++ show sexy0 ++ " and " ++ err
sexyCouple _ sexy0 sexy1 =
  SexyCouple (sexy0, sexy1)

-- sexyLet e sexy0 sexy1 =
--   case evalSexy e sexy0 of
--     (_, eSexy0) -> sexyInternalLet e eSexy0 sexy1

sexyLet e (SexyCouple (SexyAtom key, sexyToBeBounded@(SexyError _))) arg1 =
  SexyError
    $ "(SexyEnv: " ++ show e ++ "), failed at binding of " ++ show sexyToBeBounded ++ " by " ++ show key ++ " and failed to do " ++ show arg1
sexyLet e (SexyCouple (SexyAtom key, sexyToBeBounded)) sexy =
  (\x -> (evalSexy x sexy))
  $ setSexyVar e key sexyToBeBounded
sexyLet e arg0 arg1 =
  SexyError
    $ "(SexyEnv: " ++ show e ++ "), the 1st argument was not SexyCouple but " ++ show arg0 ++ " and the 2nd argument was " ++ show arg1

showSexy :: Sexy -> String
-- showSexy (SexyCommand ("+", Function _))    = "(Function: addition)"
-- showSexy (SexyCommand ("-", Function _))    = "(Function: subtraction)"
-- showSexy (SexyCommand ("*", Function _))    = "(Function: mutiplication)"
-- showSexy (SexyCommand ("/", Function _))    = "(Function: division)"
-- showSexy (SexyCommand ("%", Function _))    = "(Function: modulo)"
-- showSexy (SexyCommand (val, Function _))    = "(Function: " ++ val ++ ")"
-- showSexy (SexyCommand ("&", SpecialForm _)) = "(SpecialForm: don't evaluate the first argument and bind)"
-- showSexy (SexyCommand ("@", SpecialForm _)) = "(SpecialForm: evaluate the first argument and bind)"
-- showSexy (SexyCommand ("d", SpecialForm _))   = "(SpecialForm: debind)"
-- showSexy (SexyCommand (val, SpecialForm _))   = "(SpecialForm: " ++ val ++ ")"
-- showSexy (SexyCommand (func, UnknownCommand)) = "(UnknownCommand: " ++ func ++ ")"
-- showSexy (LazyNonNorm  (func, arg0, arg1)) =
--   "(LazyNonNorm: " ++ showSexy func ++ " " ++ show arg0 ++ " " ++ show arg1 ++")"
showSexy (NonNorm  (func, arg0, arg1)) =
  "(NonNorm: " ++ showSexy func ++ " " ++ show arg0 ++ " " ++ show arg1 ++")"
showSexy (SexyFunction _)              = "(SexyFunction)"
-- showSexy (SexySpecialForm _)              = "(SexySpecialForm)"
showSexy (SexyAtom sexyKey)            = "(SexyAtom: " ++ sexyKey ++ ")"
showSexy (SexyInteger integer)         = "(SexyInteger: " ++ show integer ++ ")"
showSexy (SexyText text)               = "(SexyText: " ++ show text ++ ")"
showSexy (SexyBool bool)               = "(SexyBool: " ++ show bool ++ ")"
showSexy (SexyCouple (sexy0, sexy1))   = "(SexyCouple: (" ++ show sexy0 ++ ", " ++ show sexy1++ "))"
showSexy SexyVoid                      = "(SexyVoid: nothing here)"
showSexy (SexyError err)               = "(SexyError: " ++ err ++ ")"
showSexy EOF                           = "(EOF: end of file)"

instance Show Sexy where show = showSexy

evalSexy :: SexyEnv -> Sexy -> Sexy
evalSexy e sexy =
  case sexy of
    NonNorm ((SexyFunction sexyfunc), arg0, arg1) ->
      case (evalSexy e arg0, evalSexy e arg1) of
        (eArg0, eArg1) ->
          -- (\(_, x) -> evalSexy e x)
          (sexyfunc e eArg0 eArg1)
    NonNorm (sexy0, sexy1, sexy2) ->
      case evalSexy e sexy0 of
        eSexy0 -> evalSexy e (NonNorm (eSexy0, sexy1, sexy2))
    LazyNonNorm nonnorm -> NonNorm nonnorm
    val@(SexyAtom sexyKey) ->
      May.fromMaybe (val) $ e !? sexyKey
    val -> val

repSexy :: SexyEnv -> Maybe String -> HLine.InputT IO ()
repSexy sexyEnv (Just input) = do
    (\y -> do {HLine.outputStrLn . TLazy.unpack . PrettyS.pShow $ y
                   ; replSexy sexyEnv})
      $ evalSexy sexyEnv
      -- HLine.outputStrLn . TLazy.unpack . PrettyS.pShow
      $ E.either (SexyError . show) id $ P.parse parseSexy "" input
repSexy _ Nothing = do
  HLine.outputStrLn . TLazy.unpack . PrettyS.pString $ "(SexyFarewell: Bye!)"

replSexy :: SexyEnv -> HLine.InputT IO ()
replSexy sexyEnv = do
  maybeInput <- HLine.getInputLine "(SexyEval:)>> "
  repSexy sexyEnv maybeInput

main :: IO ()
main = do
  args <- Env.getArgs
  HLine.runInputT HLine.defaultSettings $ May.maybe
    (replSexy sexyFuncs)
    (\x -> repSexy (Map.empty) (Just x))
    $ Safe.headMay args
