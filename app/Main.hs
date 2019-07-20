module Main where

-- import           Lib
-- import           System.Environment
-- import qualified System.IO                     as IO
-- import qualified Safe                          as Safe
import qualified Data.Either                   as E
import qualified Data.IORef                    as IORef
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as May
import qualified Data.Text.Lazy                as TLazy
import qualified System.Console.Haskeline      as HLine
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as PCP
import qualified Text.Pretty.Simple            as PrettyS

type SexyKey = String

type SexyEnv = Map.Map SexyKey Sexy

type SexyReturn = (SexyEnv, Sexy)

-- type SexyEnv = IORef.IORef [(SexyKey, IORef.IORef Sexy)]

data Sexy = NonNorm  (SexyFunc, Sexy, Sexy)
          | Norm     Value

data SexyFunc = Arthmetic (SexyKey, (SexyEnv -> Sexy -> Sexy -> SexyReturn))
              | UnknownFunc String

data Value = SexyInteger Integer
           | SexyAtom    SexyKey
           | SexyError   String
           | EOF
           -- | SexyBool    Bool

-- newSexyEnv :: IO SexyEnv
-- newSexyEnv = IORef.newIORef []

-- maybeBound :: SexyEnv -> SexyKey -> IO (Maybe (IORef.IORef Sexy))
-- maybeBound sexyEnv sexyKey = lookup sexyKey <$> IORef.readIORef sexyEnv

-- setSexyVar :: SexyEnv -> SexyKey -> Sexy -> IO Sexy
-- setSexyVar sexyEnvRef sexyKey sexyToBeBounded = do
--   sexyEnv <- IORef.readIORef sexyEnvRef
--   case lookup sexyKey sexyEnv of
--     Nothing -> (return . Norm . SexyError $ "")
--     Just val -> do {flip IORef.writeIORef sexyToBeBounded val
--                    ; val}

sexyFuncs :: Map.Map SexyKey (SexyEnv -> Sexy -> Sexy -> SexyReturn)
sexyFuncs = Map.fromList [("+", sexyAdd),
                          ("-", sexySub),
                          ("*", sexyMulti),
                          ("/", sexyDiv),
                          ("%", sexyMod),
                          ("@", sexyBind)]

sexyAdd :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyAdd e (Norm (SexyInteger val1)) (Norm (SexyInteger val2)) =
  (e, (Norm . SexyInteger) (val1 + val2))
sexyAdd e arg0 arg1 =
  (e, (Norm . SexyError) $ "(Error: failed at addition of " ++ show arg0 ++ " and " ++ show arg1 ++ ")")

sexySub  :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexySub e (Norm (SexyInteger arg0)) (Norm (SexyInteger arg1)) =
  (e, (Norm . SexyInteger) (arg0 - arg1))
sexySub e arg0 arg1 =
  (e, (Norm . SexyError) $ "(Error: failed at subtraction of " ++ show arg0 ++ " by " ++ show arg1 ++ ")")

sexyMulti :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyMulti e (Norm (SexyInteger arg0)) (Norm (SexyInteger arg1)) =
  (e, (Norm . SexyInteger) (arg0 * arg1))
sexyMulti e arg0 arg1 =
  (e, (Norm . SexyError) $ "(Error: failed at muliplication of " ++ show arg0 ++ " and " ++ show arg1 ++ ")")

sexyDiv :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyDiv e (Norm (SexyInteger arg0)) (Norm (SexyInteger arg1)) =
  (e, (Norm . SexyInteger) (div arg0 arg1))
sexyDiv e arg0 arg1 =
  (e, (Norm . SexyError) $ "(Error: failed at division of " ++ show arg0 ++ " by " ++ show arg1 ++ ")")

sexyMod :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyMod e (Norm (SexyInteger arg0)) (Norm (SexyInteger arg1)) =
  (e, (Norm . SexyInteger) (mod arg0 arg1))
sexyMod e arg0 arg1 =
  (e, (Norm . SexyError) $ "(Error: failed to yield the remainder from division of " ++ show arg0 ++ " by " ++ show arg1 ++ ")")

sexyBind :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyBind e (Norm (SexyAtom sexyKey)) sexy =
  either (\v -> (e, (Norm . SexyError . show $ v))) (\x -> (x, sexy))
  $ defineSexyVar e sexyKey sexy
sexyBind e arg0 arg1 =
  (e, (Norm . SexyError) $ "(Error: failed at binding of " ++ show arg0 ++ " by " ++ show arg1 ++ ")")

sexyUnkownFunc :: SexyEnv -> Sexy -> Sexy -> SexyReturn
sexyUnkownFunc e arg0 arg1 =
  (e, (Norm . SexyError) $ "(Error: Unknown function applied to " ++ show arg0 ++ " and " ++ show arg1 ++ ")")

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
showValue (SexyAtom sexyKey)    = "(SexyAtom: " ++ sexyKey ++ ")"
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
  sexy0:sexy1:empty <- (PCP.between (PCP.spaces) (PCP.spaces) (parseSexy) <|> (PCP.spaces >> parseSexy))
                       `PCP.manyTill` (PCP.char ')')
  let
    sexyFunc = case Map.lookup funcKey sexyFuncs of
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
  do {(Norm . SexyAtom) <$> PCP.try (PCP.many1 PCP.letter)}
  <|>
  do {(Norm . SexyError . \string -> string ++ " is not an integer") <$> PCP.try (PCP.many1 PCP.letter)}

evalSexyFunc :: SexyFunc -> (SexyEnv -> Sexy -> Sexy -> SexyReturn)
evalSexyFunc (Arthmetic (_, sexyFunc)) = sexyFunc
evalSexyFunc (UnknownFunc _)           = sexyUnkownFunc

evalSexy :: SexyEnv -> Sexy -> SexyReturn
evalSexy e sexy =
  case sexy of
    NonNorm (sexyFunc, sexy1, sexy2) ->
      (\(x, y, z) -> evalSexyFunc sexyFunc x y z)
      (case ((evalSexy e sexy1), (evalSexy e sexy2)) of
         ((env0, val0), (env1, val1)) -> ((Map.unions [e, env0, env1]), val0, val1))
    val@(Norm (SexyInteger _))    -> (e, val)
    val@(Norm (SexyError _))      -> (e, val)
    val@(Norm (SexyAtom sexyKey)) ->
      May.maybe (e, val) (\x -> (evalSexy e x)) $ Map.lookup sexyKey e
    val@(Norm EOF)                -> (e, val)


-- flushStr :: String -> IO ()
-- flushStr str = putStr str >> IO.hFlush IO.stdout

initSexyEnv :: Map.Map SexyKey Sexy
initSexyEnv = Map.empty

setSexyVar :: SexyEnv -> SexyKey -> Sexy -> SexyEnv
setSexyVar sexyEnv sexyKey sexyToBeBounded =
  Map.insert sexyKey sexyToBeBounded sexyEnv

defineSexyVar :: SexyEnv -> SexyKey -> Sexy -> Either Sexy SexyEnv
defineSexyVar sexyEnv sexyKey sexyToBeBounded =
  May.maybe
  (Right $ Map.insert sexyKey sexyToBeBounded sexyEnv)
  (\x -> Left . Norm . SexyError $ "sexy" ++ show x)
  (sexyEnv Map.!? sexyKey)

repSexy :: SexyEnv -> Maybe String -> HLine.InputT IO ()
repSexy sexyEnv (Just input) = do
    (\(x, y) -> do {HLine.outputStrLn . TLazy.unpack . PrettyS.pShow $ (x,y)
                   ; replSexy x})
      $ evalSexy sexyEnv
      $ E.either (Norm . SexyError . show) id $ P.parse parseSexy "" input
      -- case (parse parseSexy "" input) of
      --   Right sexy -> sexy
      --   Left err   -> (Norm . SexyError) (show err)

repSexy _ Nothing = do
  HLine.outputStrLn . TLazy.unpack . PrettyS.pString $ "(SexyFarewell: Bye!)"

replSexy :: SexyEnv -> HLine.InputT IO ()
replSexy sexyEnv = do
  maybeInput <- HLine.getInputLine "(SexyEval:)>> "
  repSexy sexyEnv maybeInput

main :: IO ()
main = do
  HLine.runInputT HLine.defaultSettings $ replSexy Map.empty
