module Main where

-- import           Lib
-- import qualified Data.IORef                    as IORef
-- import qualified System.IO                     as IO
import qualified Data.Either                   as E
import qualified Data.List                     as List
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
          | SexyFunction (SexyEnv -> Sexy -> Sexy -> (SexyEnv, Sexy))
          | SexySpecialForm (SexyEnv -> Sexy -> Sexy -> (SexyEnv, Sexy))
          | SexyClosure (SexyKey, SexyEnv -> Sexy -> Sexy -> (SexyEnv, Sexy))
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
    -- , ("@", SexyFunction sexyBind)
    -- , ("@", sexyEvalBind)
    -- , ("d", SexyFunction sexyDeBind)
    , ("couple", SexyFunction sexyCouple)
    -- , (".", SexyFunction sexyCouple)
    , ("do", SexySpecialForm sexyDo)
    , ("let", SexySpecialForm sexyLet)
    -- , ("text", sexyTextConstructer)
    ]

symbol :: PCP.Parser Char
symbol = PCP.oneOf "!#$%&|*+-/:<=>?@^~"

parseSexy, parseNonNorm, parseCouple, parseAtom, parseInteger, parseText, parseBool, parseVoid
  :: PCP.Parser Sexy

parseSexy = PCP.spaces >>
  PCP.choice [ PCP.try parseNonNorm
             , PCP.try parseCouple
             , PCP.try parseAtom
             , PCP.try parseInteger
             , PCP.try parseText
             , PCP.try parseBool
             , PCP.try parseVoid ]

parseNonNorm = do
  _       <- PCP.spaces >> PCP.char '('
  command <- PCP.spaces >> parseAtom
  arg0    <- PCP.spaces >> parseSexy
  arg1    <- PCP.spaces >> parseSexy
  _       <- PCP.spaces >> PCP.char ')'
  return $ NonNorm (command, arg0, arg1)

parseCouple = do
  _       <- PCP.spaces >> PCP.char '('
  _ <- PCP.spaces >> PCP.char '.'
  arg0    <- PCP.spaces >> parseSexy
  arg1    <- PCP.spaces >> parseSexy
  _       <- PCP.spaces >> PCP.char ')'
  return $ SexyCouple (arg0, arg1)

-- parseCommand = do
--   atom <- P.try $ PCP.choice
--           $ map (P.try . PCP.string) (List.reverse . List.sort . Map.keys $ sexyFuncs)
--   return $ May.fromJust $ Map.lookup atom sexyFuncs

parseAtom =
  PCP.try $ do
    x <- PCP.letter
    xs <- PCP.many $ PCP.try PCP.letter <|> PCP.try PCP.digit <|> symbol
    return $ SexyAtom $ x:xs
  <|> SexyAtom <$> PCP.many1 symbol

-- parseAtom = SexyAtom <$> PCP.many1 (PCP.noneOf " )")

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

parseBool = do
  tOrF <- PCP.oneOf "TF"
  return . SexyBool
    $ case tOrF of
        'T' -> True
        _   -> False

parseVoid = do
  _ <- PCP.char '_'
  return SexyVoid

sexyAdd, sexySub, sexyMulti, sexyDiv, sexyMod, sexyIf, sexyIsSuccess, sexyLet, sexyDo, sexyCouple
  :: SexyEnv -> Sexy -> Sexy -> (SexyEnv, Sexy)

sexyAdd e (SexyInteger val1) (SexyInteger val2) =
  (e, SexyInteger $ val1 + val2)
sexyAdd e arg0 arg1 =
  (e, SexyError $ "failed at addition of " ++ show arg0 ++ " and " ++ show arg1)

sexySub e (SexyInteger arg0) (SexyInteger arg1) =
  (e, SexyInteger $ arg0 - arg1)
sexySub e arg0 arg1 =
  (e, SexyError $ "failed at subtraction of " ++ show arg0 ++ " by " ++ show arg1)

sexyMulti e (SexyInteger arg0) (SexyInteger arg1) =
  (e, SexyInteger $ arg0 * arg1)
sexyMulti e arg0 arg1 =
  (e, SexyError $ "failed at muliplication of " ++ show arg0 ++ " and " ++ show arg1)

sexyDiv e (SexyInteger arg0) (SexyInteger arg1) =
  (e, SexyInteger $ div arg0 arg1)
sexyDiv e arg0 arg1 =
  (e, SexyError $ "failed at division of " ++ show arg0 ++ " by " ++ show arg1)

sexyMod e (SexyInteger arg0) (SexyInteger arg1) =
  (e, SexyInteger $ mod arg0 arg1)
sexyMod e arg0 arg1 =
  (e, SexyError $ "failed to yield the remainder from division of " ++ show arg0 ++ " by " ++ show arg1)

sexyIf e (SexyBool bool) (SexyCouple (thethen, theelse)) =
  if bool then ((evalSexy e thethen)) else (evalSexy e theelse)
sexyIf e arg0 arg1 =
  (e, SexyError $ "the first arg was " ++ show arg0 ++ " but the second argument were not SexyCouple but " ++ show arg1)

sexyIsSuccess e SexyVoid sexy =
  (e, case sexy of
      SexyError _ -> SexyBool False
      _           -> SexyBool True)
sexyIsSuccess e arg0 arg1 =
  (e, SexyError $ "the first argument is not SexyVoid but " ++ show arg0 ++ "; the second argument is " ++ show arg1)

sexyEvalText :: SexyEnv -> Sexy -> Sexy -> (SexyEnv, Sexy)
sexyEvalText e SexyVoid (SexyText code) =
  (evalSexy e $ E.either (SexyError . show) id $ P.parse parseSexy "" (TLazy.unpack code))
sexyEvalText e SexyVoid arg1 =
  (e, SexyError
    $ "the first argument was SexyVoid but the second argument was not SexyText but " ++ show arg1)
sexyEvalText e arg0 arg1 =
  (e, SexyError
    $ "the first argument is not SexyVoid but " ++ show arg0 ++ "; the second argument is " ++ show arg1)

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
    (_, sexy@(SexyError _)) ->
      (e, SexyError $ "failed doing "
        ++ show arg0 ++ " with error " ++ show sexy
        ++ " before doing " ++ show arg1)
    (env0, _) -> (evalSexy env0 arg1)
sexyDo e arg0 arg1 =
  (e, SexyError
    $ "failed at doing " ++ show arg0 ++ " and " ++ show arg1)

sexyCouple e (SexyError err) sexy1 =
  (e, SexyError $ "can't make couple with " ++ err ++ " and " ++ show sexy1)
sexyCouple e sexy0 (SexyError err) =
  (e, SexyError $ "can't make couple with " ++ show sexy0 ++ " and " ++ err)
sexyCouple e sexy0 sexy1 =
  (e, SexyCouple (sexy0, sexy1))

sexyLet e (SexyCouple (SexyAtom key, sexyToBeBounded@(SexyError _))) arg1 =
  (e, SexyError
    $ "failed at binding of " ++ show sexyToBeBounded ++ " by " ++ show key ++ " and failed to do " ++ show arg1)
sexyLet e (SexyCouple (SexyAtom key, sexyToBeBounded)) sexy =
  E.either (\v -> (e, (SexyError . show $ v))) (\x -> (evalSexy x sexy))
  $ defineSexyVar e key sexyToBeBounded
sexyLet e arg0 arg1 =
  (e, SexyError
    $ "the 1st argument was not SexyCouple but " ++ show arg0 ++ " and the 2nd argument was " ++ show arg1)


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
showSexy (SexySpecialForm _)              = "(SexySpecialForm)"
showSexy (SexyAtom sexyKey)            = "(SexyAtom: " ++ sexyKey ++ ")"
showSexy (SexyInteger integer)         = "(SexyInteger: " ++ show integer ++ ")"
showSexy (SexyText text)               = "(SexyText: " ++ show text ++ ")"
showSexy (SexyBool bool)               = "(SexyBool: " ++ show bool ++ ")"
showSexy (SexyCouple (sexy0, sexy1))   = "(SexyCouple: (" ++ show sexy0 ++ ", " ++ show sexy1++ "))"
showSexy SexyVoid                      = "(SexyVoid: nothing here)"
showSexy (SexyError err)               = "(SexyError: " ++ err ++ ")"
showSexy EOF                           = "(EOF: end of file)"

instance Show Sexy where show = showSexy

-- matchSexyCommand :: SexyKey -> Value -- (SexyEnv -> Sexy -> Sexy -> (SexyEnv ,Sexy))
-- matchSexyCommand key = SexyCommand
--                        $ case (Map.lookup key sexyFuncs, Map.lookup key sexySForms) of
--                            (Nothing, Just spForm) -> (key, spForm)
--                            (Just func, _)         -> (key, func)
--                            (_, _)                 -> (key, UnknownCommand)
-- -- evalSexyCommand (Function (_, sexyFunc))  = sexyFunc
-- -- evalSexyCommand (SpecialForm (_, spForm)) = spForm
-- -- evalSexyCommand (UnknownCommand _)        = sexyUnkownFunc

evalSexy :: SexyEnv -> Sexy -> (SexyEnv, Sexy)
evalSexy e sexy =
  case sexy of
    NonNorm ((SexyAtom atom), sexy0, sexy1) ->
      May.maybe
      (e, SexyError
        $ "invalid Function or invalid SpecialForm " ++ atom ++ " got arguments " ++ show sexy0 ++ " and " ++ show sexy1)
      (\x -> matchCommands e x sexy0 sexy1)
      $ e !? atom
    val@(SexyAtom sexyKey) ->
      (e, May.fromMaybe (val) $ e !? sexyKey)
    val -> (e, val)

matchCommands :: SexyEnv -> Sexy -> Sexy -> Sexy -> (SexyEnv, Sexy)
matchCommands e (SexyFunction func) sexy0 sexy1 =
  case (evalSexy e sexy0, evalSexy e sexy1) of
    ((_, eSexy0), (_, eSexy1)) ->
      case (func e eSexy0 eSexy1) of
        (_, ret) -> (e, ret)
matchCommands e (SexySpecialForm spform) sexy0 sexy1 =
  case (spform e sexy0 sexy1) of
       (_, ret) -> (e, ret)
matchCommands e atom sexy0 sexy1 =
  (e, SexyError
        $ "invalid Function or invalid SpecialForm " ++ show atom ++ " got arguments " ++ show sexy0 ++ " and " ++ show sexy1)

repSexy :: SexyEnv -> Maybe String -> HLine.InputT IO ()
repSexy sexyEnv (Just input) = do
    (\(x, y) -> do {HLine.outputStrLn . TLazy.unpack . PrettyS.pShow $ y
                   ; replSexy x})
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
