module Main where

-- import           Lib
-- import qualified Data.IORef                    as IORef
-- import qualified System.IO                     as IO
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

type SexyEnv = Map.Map SexyKey Sexy

type SexyReturn = (SexyEnv, Sexy)

data SexyError = Message String
               | EOF

data Sexy = NonNorm  (SexyCommand, Sexy, Sexy)
          | Norm     (Either SexyError Value)

data SexyCommand = Function    (SexyKey, (SexyEnv -> Sexy -> Sexy -> SexyReturn))
                 | SpecialForm (SexyKey, (SexyEnv -> Sexy -> Sexy -> SexyReturn))
                 | UnknownCommand String

data Value = SexyAtom    SexyKey
           | SexyInteger Integer
           | SexyText    TLazy.Text
           | SexyBool    Bool
           | SexyCouple   (Sexy, Sexy)
           | SexyVoid
           -- | EOF

sexyFuncs :: Map.Map SexyKey (SexyEnv -> Sexy -> Sexy -> SexyReturn)
sexyFuncs = Map.fromList [("+", sexyAdd),
                          ("-", sexySub),
                          ("*", sexyMulti),
                          ("/", sexyDiv),
                          ("%", sexyMod),
                          ("if", sexyIf),
                          ("?", sexyIsSuccess)]

sexySForms :: Map.Map SexyKey (SexyEnv -> Sexy -> Sexy -> SexyReturn)
sexySForms = Map.fromList [("@", sexyBind),
                           ("eval@", sexyEvalBind),
                           ("do", sexyDo),
                           ("d", sexyDeBind),
                           ("couple", sexyCouple),
                           (".", sexyCouple),
                           ("text", sexyTextConstructer)
                           -- ("eval", sexyEval)
                          ]

sexyCommands :: Map.Map SexyKey (SexyEnv -> Sexy -> Sexy -> SexyReturn)
sexyCommands = Map.unions [sexyFuncs, sexySForms]

sexyReserved :: Map.Map SexyKey Sexy
sexyReserved = Map.fromList [("_", Norm . Right $ SexyVoid),
                             ("T", Norm . Right . SexyBool $ True),
                             ("F", Norm . Right . SexyBool $ False)]

sexyAdd :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyAdd e (Norm (Right (SexyInteger val1))) (Norm (Right (SexyInteger val2))) =
  (e, Norm . Right . SexyInteger $ val1 + val2)
sexyAdd e arg0 arg1 =
  (e, Norm . Left . Message $ "failed at addition of " ++ show arg0 ++ " and " ++ show arg1)

sexySub  :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexySub e (Norm (Right (SexyInteger arg0))) (Norm (Right (SexyInteger arg1))) =
  (e, Norm . Right . SexyInteger $ arg0 - arg1)
sexySub e arg0 arg1 =
  (e, Norm . Left . Message $ "failed at subtraction of " ++ show arg0 ++ " by " ++ show arg1)

sexyMulti :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyMulti e (Norm (Right (SexyInteger arg0))) (Norm (Right (SexyInteger arg1))) =
  (e, Norm . Right . SexyInteger $ arg0 * arg1)
sexyMulti e arg0 arg1 =
  (e, Norm . Left . Message $ "failed at muliplication of " ++ show arg0 ++ " and " ++ show arg1)

sexyDiv :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyDiv e (Norm (Right (SexyInteger arg0))) (Norm (Right (SexyInteger arg1))) =
  (e, Norm . Right . SexyInteger $ div arg0 arg1)
sexyDiv e arg0 arg1 =
  (e, Norm . Left . Message $ "failed at division of " ++ show arg0 ++ " by " ++ show arg1)

sexyMod :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyMod e (Norm (Right (SexyInteger arg0))) (Norm (Right (SexyInteger arg1))) =
  (e, Norm . Right . SexyInteger $ mod arg0 arg1)
sexyMod e arg0 arg1 =
  (e, Norm . Left . Message $ "failed to yield the remainder from division of " ++ show arg0 ++ " by " ++ show arg1)

sexyIf :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyIf e (Norm (Right (SexyBool bool))) (Norm (Right (SexyCouple (thethen, theelse)))) =
  if bool then (evalSexy e thethen) else (evalSexy e theelse)
sexyIf e arg0 arg1 =
  (e, Norm . Left . Message $ "the first arg was " ++ show arg0 ++ " but the second argument were not SexyCouple but " ++ show arg1)

sexyIsSuccess :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyIsSuccess e (Norm (Right SexyVoid)) (Norm sexy) =
  (e, Norm . Right $ case sexy of
                   Right _ -> SexyBool True
                   Left _  -> SexyBool False)
sexyIsSuccess e arg0 arg1 =
  (e, Norm . Left . Message $ "the first argument is not SexyVoid but " ++ show arg0 ++ "; the second argument is " ++ show arg1)

setSexyVar :: SexyEnv -> SexyKey -> Sexy -> SexyEnv
setSexyVar sexyEnv sexyKey sexyToBeBounded =
  Map.insert sexyKey sexyToBeBounded sexyEnv

defineSexyVar :: SexyEnv -> SexyKey -> Sexy -> Either Sexy SexyEnv
defineSexyVar e sexyKey sexyToBeBounded =
  May.maybe
  (Right $ Map.insert sexyKey sexyToBeBounded e)
  (\x -> Left . Norm . Left . Message $ show x ++ " already binded")
  ((Map.unions [sexyReserved , e]) !? sexyKey)

sexyBind :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyBind e arg0@(Norm (Left _)) arg1@(Norm (Right (SexyAtom _))) =
  (e, Norm . Left . Message $ "failed at binding of " ++ show arg0 ++ " by " ++ show arg1)
sexyBind e sexy (Norm (Right (SexyAtom sexyKey))) =
  E.either (\v -> (e, (Norm . Left . Message . show $ v))) (\x -> (x ,sexy))
  $ defineSexyVar e sexyKey sexy
sexyBind e arg0 arg1 =
  (e, Norm . Left . Message $ "failed at binding of " ++ show arg0 ++ " by " ++ show arg1)

sexyEvalBind :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyEvalBind e arg0 arg1 =
  case (evalSexy e arg0) of
    (_, evaluated) -> sexyBind e evaluated arg1


sexyDeBind :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyDeBind e (Norm (Right (SexyVoid))) var@(Norm (Right (SexyAtom sexyKey))) =
  if May.isJust (e !? sexyKey)
  then ((Map.delete sexyKey e), var)
  else (e, (Norm . Left . Message $ sexyKey ++ " is not binded"))
sexyDeBind e (Norm (Right SexyVoid)) arg1 =
  (e, (Norm . Left . Message) $ "failed at deleting binding of by " ++ show arg1)
sexyDeBind e arg0 val@(Norm (Right (SexyAtom _))) =
  (e, (Norm . Left . Message)
    $ "failed at deleting binding because the arguments was "
    ++ show arg0 ++ " and " ++ show val
    ++ "; the first argument should be SexyVoid")
sexyDeBind e arg0 arg1 =
  (e, (Norm . Left . Message)
    $ "failed at deleting binding because the arguments was not SexyVoid and some atom but "
    ++ show arg0 ++ " and " ++ show arg1)

sexyDo :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyDo e arg0@(NonNorm _) arg1@(NonNorm _) =
  case (evalSexy e arg0) of
    (_, sexy@(Norm (Left _))) ->
      (e, Norm . Left . Message $ "failed doing "
        ++ show arg0 ++ " with error " ++ show sexy
        ++ " before doing " ++ show arg1)
    (env0, _) -> (evalSexy env0 arg1)
sexyDo e arg0 arg1 =
  (e, (Norm . Left . Message)
    $ "failed at doing " ++ show arg0 ++ " and " ++ show arg1)

sexyCouple :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyCouple e (Norm (Left (Message err))) sexy1 =
  (e, (Norm . Left . Message) $ "can't make couple with " ++ err ++ " and " ++ show sexy1)
sexyCouple e sexy0 (Norm (Left (Message err))) =
  (e, (Norm . Left . Message) $ "can't make couple with " ++ show sexy0 ++ " and " ++ err)
sexyCouple e sexy0 sexy1 =
  (e, (Norm . Right . SexyCouple) (sexy0, sexy1))

-- sexyEval :: SexyEnv -> Sexy -> Sexy -> SexyReturn
-- sexyEval e =

sexyTextConstructer :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyTextConstructer e (Norm (Right SexyVoid)) (Norm (Right (SexyAtom arg1))) =
  (e, (Norm . Right . SexyText . TLazy.pack) $ arg1)
sexyTextConstructer e arg0 (Norm (Right (SexyAtom arg1))) =
  (e, (Norm . Left . Message)
    $ "the first argument should be SexyVoid but was "
    ++ show arg0
    ++ "; failed to construct text from " ++ arg1)
sexyTextConstructer e arg0 arg1 =
  (e, (Norm . Left . Message)
    $ "failed at deleting binding because the arguments was not SexyVoid and some atom but "
    ++ show arg0 ++ " and " ++ show arg1)



sexyUnkownFunc :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyUnkownFunc e arg0 arg1 =
  (e, (Norm . Left. Message) $ " Unknown function applied to " ++ show arg0 ++ " and " ++ show arg1 ++ ")")

showSexy :: Sexy -> String
showSexy (NonNorm (sexyfunc, sexy1, sexy2)) =
  showSexyCommand sexyfunc ++ " (Sexy: " ++ showSexy sexy1 ++ ") (Sexy: " ++ showSexy sexy2 ++ ")"
showSexy (Norm (Right value)) = "(Value: " ++ show value ++ ")"
showSexy (Norm (Left value)) = "(SexyError: " ++ show value ++ ")"

instance Show Sexy where show = showSexy

showSexyCommand :: SexyCommand -> String
showSexyCommand (Function ("+", _))    = "(Function: addition)"
showSexyCommand (Function ("-", _))    = "(Function: subtraction)"
showSexyCommand (Function ("*", _))    = "(Function: mutiplication)"
showSexyCommand (Function ("/", _))    = "(Function: division)"
showSexyCommand (Function ("%", _))    = "(Function: modulo)"
showSexyCommand (Function (val, _))    = "(Function: " ++ val ++ ")"
showSexyCommand (SpecialForm ("@", _)) = "(SpecialForm: bind)"
showSexyCommand (SpecialForm ("d", _)) = "(SpecialForm: debind)"
showSexyCommand (SpecialForm (val, _)) = "(SpecialForm: " ++ val ++ ")"
showSexyCommand (UnknownCommand func)  = "(Unknown function: " ++ func ++ ")"

instance Show SexyCommand where show = showSexyCommand

showValue :: Value -> String
showValue (SexyAtom sexyKey)          = "(SexyAtom: " ++ sexyKey ++ ")"
showValue (SexyInteger integer)       = "(SexyInteger: " ++ show integer ++ ")"
showValue (SexyText text)             = "(SexyText: " ++ show text ++ ")"
showValue (SexyBool bool)             = "(SexyBool: " ++ show bool ++ ")"
showValue (SexyCouple (sexy0, sexy1)) = "(SexyCouple: (" ++ show sexy0 ++ ", " ++ show sexy1++ "))"
showValue SexyVoid                    = "(SexyVoid: nothing here)"

instance Show Value where show = showValue

showSexyError :: SexyError -> String
showSexyError (Message err) = "(Message: " ++ err ++ ")"
showSexyError EOF           = "(EOF: end of file)"

instance Show SexyError where show = showSexyError

parseSexy :: PCP.Parser Sexy
parseSexy = do -- spaces >> (node <|> parsed)
     (do { PCP.eof
         ; return $ (Norm . Left) EOF})
       <|>
       (PCP.spaces >>
            (parseSexyNonNorm <|> parseSexyNorm))

parseSexyNonNorm :: PCP.Parser Sexy
parseSexyNonNorm = do
  _ <- PCP.char '('
  commKey <- PCP.spaces >>
    do { x <- PCP.anyToken
       ; xs <- ((PCP.letter <|> (PCP.oneOf (foldl1 (++) $ Map.keys sexyCommands)))
                 `PCP.manyTill` (PCP.lookAhead
                                  (P.try
                                    ((PCP.many1 PCP.space)
                                      <|> (P.try (PCP.many1 $ PCP.char '('))
                                      <|> (P.try (PCP.many1 PCP.digit))
                                      <|> (P.try (PCP.choice $ map PCP.string (Map.keys sexyReserved)))))))
       ; return $ x:xs}
  args <- ((do {x <- PCP.spaces >> parseSexy
               ; PCP.spaces
               ; return x})
            `PCP.manyTill` PCP.char ')')
  let
    sexyComm = case ((Map.lookup commKey sexyFuncs), (Map.lookup commKey sexySForms)) of
                 (Just commKeyhole, _) -> (Function (commKey, commKeyhole))
                 (_, Just commKeyhole) -> (SpecialForm (commKey, commKeyhole))
                 (_, _)    -> (UnknownCommand $ commKey)
    in
    return $ case length args of
               2 -> NonNorm (sexyComm,
                             (May.fromMaybe (Norm . Left . Message $ "how can ths be error?") $ Safe.headMay args),
                             (May.fromMaybe (Norm . Left . Message$ "how can ths be error?") $ Safe.lastMay args))
               thelength -> (Norm . Left . Message)
                            $ show sexyComm ++ " took not 2 arguments but " ++ show thelength
                            ++ (foldl (\xs x -> xs ++ " " ++ x) " arguments:" $ map show args)

parseSexyNorm :: PCP.Parser Sexy
parseSexyNorm = do
  ((Norm . Right . SexyInteger . read) <$> PCP.try (PCP.many1 PCP.digit))
    <|> -- Reserved
    ((\k -> May.fromMaybe (Norm . Left . Message $ k ++ " is not reserved")
       (Map.lookup k sexyReserved))
      <$> PCP.try (PCP.choice $ map PCP.string (Map.keys sexyReserved)))
    <|> ((Norm . Right . SexyAtom) <$> PCP.try (P.try $ PCP.many1 ((P.try parseEscape) <|> PCP.noneOf ")")))

parseEscape :: PCP.Parser Char
parseEscape = do
  _ <- PCP.char '\\'
  PCP.anyToken

evalSexyCommand :: SexyCommand -> (SexyEnv -> Sexy -> Sexy -> SexyReturn)
evalSexyCommand (Function (_, sexyFunc))  = sexyFunc
evalSexyCommand (SpecialForm (_, spForm)) = spForm
evalSexyCommand (UnknownCommand _)        = sexyUnkownFunc

evalSexy :: SexyEnv -> Sexy -> SexyReturn
evalSexy e sexy =
  case sexy of
    NonNorm (func@(Function _), sexy0, sexy1)
      -> (\(x, y, z) -> evalSexyCommand func x y z)
         $ case ((evalSexy e sexy0), (evalSexy e sexy1)) of
             ((_, val0), (_, val1)) -> (-- (Map.unions [env1, env0, e])
               e, val0, val1)
    NonNorm (spForm@(SpecialForm _), sexy0, sexy1)
      -> (evalSexyCommand spForm e sexy0 sexy1)
    -- val@(Norm (SexyInteger _))    -> (e, val)
    NonNorm (unknown@(UnknownCommand _), sexy0, sexy1)
      -> (evalSexyCommand unknown e sexy0 sexy1)
    val@(Norm (Right (SexyAtom sexyKey))) ->
      May.maybe (e, val) (evalSexy e) $ Map.lookup sexyKey (Map.unions [e, sexyReserved])
    val -> (e, val)
    -- val@(Norm (SexyCouple _))       -> (e, val)
    -- val@(Norm (SexyError _))      -> (e, val)
    -- val@(Norm SexyVoid)           -> (e, val)
    -- val@(Norm EOF)                -> (e, val)

-- flushStr :: String -> IO ()
-- flushStr str = putStr str >> IO.hFlush IO.stdout

repSexy :: SexyEnv -> Maybe String -> HLine.InputT IO ()
repSexy sexyEnv (Just input) = do
    (\(x, y) -> do {HLine.outputStrLn . TLazy.unpack . PrettyS.pShow $ (x,y)
                   ; replSexy x})
      $ evalSexy sexyEnv
  -- HLine.outputStrLn . TLazy.unpack . PrettyS.pShow
      $ E.either (Norm . Left . Message . show) id $ P.parse parseSexy "" input
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
    (replSexy Map.empty)
    (\x -> repSexy Map.empty (Just x))
    $ Safe.headMay args
