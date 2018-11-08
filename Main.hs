{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-
TODO:
    - Run tests concurrently
    - Run `elm make --docs $(mktemp --suffix .json)` if flag isn't provided
    - Remove undefined branches
-}
module Main (main) where

import           Prelude

import qualified Control.Exception      as Exception
import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.Types       as Aeson (Parser)
import qualified Data.Attoparsec.Text   as Parse
import qualified Data.Either            as Either
import qualified Data.List              as List
import qualified Data.Maybe             as Maybe
import qualified Data.Text              as Text
import qualified System.Console.ANSI    as ANSI
import qualified System.Console.CmdArgs as CmdArgs
import qualified System.IO              as IO
import qualified System.Process         as Process

import           Control.Applicative    ((<|>))
import           Control.Monad          ((>=>))
import           Data.Aeson             ((.:))
import           Data.Data              (Data)
import           Data.Foldable          (traverse_)
import           Data.Functor           (($>))
import           Data.Text              (Text)
import           System.Console.CmdArgs ((&=))


newtype Flags = Flags
    { docs :: Maybe FilePath
    } deriving Data


flagSpec :: Flags
flagSpec =
    Flags
            (  CmdArgs.def
            &= CmdArgs.help "Elm documentation (json)"
            &= CmdArgs.typFile
            )
        &= CmdArgs.summary "elm-doctest"


main :: IO ()
main = do
    flags            <- CmdArgs.cmdArgs flagSpec
    moduleDocs       <- getModuleDocs flags
    (errors, passes) <- Either.partitionEithers
        <$> traverse check (extractDocTests moduleDocs)

    -- Report how we did
    print . Pretty Green $ showText (length passes) <> " passes"
    print . Pretty Red $ showText (length errors) <> " errors"
    putChar '\n'
    traverse_ printError errors
  where
    check :: DocTest -> IO (Either Error Feedback)
    check docTest = withElmRepl $ runDocTest docTest >=> pure . \case
        AllGood               -> Right (feedbackOk docTest)

        OutputMismatch got    -> Left (outputMismatchError docTest got)

        ReplError      stderr -> Left (replError docTest stderr)

    printError :: Error -> IO ()
    printError err = do
        putStrLn . concatMap show . unwrapError $ err
        putChar '\n'


getModuleDocs :: Flags -> IO [ElmModuleDoc]
getModuleDocs Flags { docs = Nothing } = undefined
getModuleDocs Flags { docs = Just path } =
    Aeson.eitherDecodeFileStrict path >>= either undefined pure


feedbackOk :: DocTest -> Feedback
feedbackOk docTest = Feedback (testInput docTest <> " - ok!")


outputMismatchError :: DocTest -> Text -> Error
outputMismatchError docTest got = Error
    [ Pretty Cyan (testModule docTest <> "." <> testValue docTest)
    , Plain "\n"
    , Plain "Your example says "
    , Pretty Yellow (testInput docTest)
    , Plain " should give   "
    , Pretty Yellow (testOutput docTest)
    , Plain "\n"
    , Plain "But when I ran it "
    , Pretty Yellow (testInput docTest)
    , Plain " actually gave "
    , Pretty Yellow got
    , Plain "\n"
    ]


replError :: DocTest -> Text -> Error
replError docTest err = Error
    [ Pretty Cyan (testModule docTest <> "." <> testValue docTest)
    , Plain "\n"
    , Plain err
    ]


data PrettyText
    = Pretty Color Text
    | Plain Text


_unPretty :: PrettyText -> Text
_unPretty (Pretty _ text) = text
_unPretty (Plain text   ) = text


instance Show PrettyText where
    show :: PrettyText -> String
    show (Plain text) = Text.unpack text
    show (Pretty color text) = mconcat
        [ ANSI.setSGRCode [colorCode color]
        , Text.unpack text
        , ANSI.setSGRCode [ANSI.Reset]
        ]


data Color = Red | Green | Cyan | Yellow


colorCode :: Color -> ANSI.SGR
colorCode Red    = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red
colorCode Green  = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green
colorCode Cyan   = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan
colorCode Yellow = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow


newtype Feedback = Feedback Text


instance Show Feedback where
    show :: Feedback -> String
    show (Feedback feedback) = Text.unpack feedback


newtype Error = Error { unwrapError :: [PrettyText] }


instance Show Error where
    show :: Error -> String
    show (Error errors) = List.intercalate ": " (fmap show errors)


instance Exception.Exception Error


-- REPL WRAPPER


data ElmRepl = ElmRepl
    { processHandle :: Process.ProcessHandle
    , stdin         :: IO.Handle
    , stdout        :: IO.Handle
    , stderr        :: IO.Handle
    }


withElmRepl :: (ElmRepl -> IO a) -> IO a
withElmRepl = Exception.bracket runElmRepl closeElmRepl


runElmRepl :: IO ElmRepl
runElmRepl = do
    (Just stdin, Just stdout, Just stderr, processHandle) <-
        Process.createProcess $ (Process.proc "elm" ["repl"])
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }
    let repl = ElmRepl {..}
    -- Consume the repl header
    _ <- getOutput repl
    pure repl


closeElmRepl :: ElmRepl -> IO ()
closeElmRepl repl = do
    -- NOTE: Order here is important!
    IO.hClose (stdin repl)
    _ <- Process.waitForProcess (processHandle repl)
    IO.hClose (stdout repl)


eval :: Text -> ElmRepl -> IO (Either Text Text)
eval expr repl = do
    putExpr (Text.unpack expr) repl
    output   <- getOutput repl
    isStderr <- IO.hReady (stderr repl)
    if isStderr
        then Left . Text.pack <$> IO.hGetContents (stderr repl)
        else Right . Text.pack <$> pure output


putExpr :: String -> ElmRepl -> IO ()
putExpr expr repl = do
    IO.hPutStrLn (stdin repl) expr
    IO.hFlush (stdin repl)


getOutput :: ElmRepl -> IO String
getOutput repl = fmap reverse $ do
    -- If an evaluated expression didn't produce any stdout then we're dropped
    -- straight back to a prompt. We need to handle that case here before we
    -- enter `go`.
    char <- IO.hGetChar (stdout repl)
    if char == prompt then IO.hGetChar (stdout repl) $> "" else go [char]
  where
    prompt :: Char
    prompt = '>'

    go :: String -> IO String
    go accum = do
        char <- IO.hGetChar (stdout repl)
        case char of
            '\n' -> do
                char' <- IO.hGetChar (stdout repl)
                if char' == prompt
                    -- There is a space immediately after the prompt, so we
                    -- consume it and throw it away
                    then IO.hGetChar (stdout repl) $> accum
                    else go (char' : char : accum)
            _ -> go (char : accum)


removeANSICodes :: Text -> Text
removeANSICodes str = case Parse.parseOnly parser (filterEscapes str) of
    Left  _        -> str
    Right scrubbed -> scrubbed
  where
    parser :: Parse.Parser Text
    parser = Text.concat
        <$> Parse.many' (ansiCode' <|> fmap Text.singleton Parse.anyChar)

    ansiCode' :: Parse.Parser Text
    ansiCode' = ansiCode $> ""

    ansiCode :: Parse.Parser ()
    ansiCode = do
        _ <- Parse.char '['
        _ <- Parse.many1 Parse.digit
        _ <- Parse.char 'm'
        pure ()

    filterEscapes :: Text -> Text
    filterEscapes = Text.filter (/= '\ESC')


-- DOCUMENTATION TYPES


data ElmModuleDoc = ElmModuleDoc
    { moduleName    :: Text
    , moduleComment :: Text
    , moduleValues  :: [ElmValueDoc]
    }


instance Aeson.FromJSON ElmModuleDoc where
    parseJSON :: Aeson.Value -> Aeson.Parser ElmModuleDoc
    parseJSON = Aeson.withObject "Module" $ \object -> do
        moduleName    <- object .: "name"
        moduleComment <- object .: "comment"
        moduleValues  <- object .: "values"
        pure ElmModuleDoc {..}


data ElmValueDoc = ElmValueDoc
    { valueName    :: Text
    , valueComment :: Text
    , valueType    :: Text
    }


instance Aeson.FromJSON ElmValueDoc where
    parseJSON :: Aeson.Value -> Aeson.Parser ElmValueDoc
    parseJSON = Aeson.withObject "Value" $ \object -> do
        valueName    <- object .: "name"
        valueComment <- object .: "comment"
        valueType    <- object .: "type"
        pure ElmValueDoc {..}


-- DOCTEST TYPES


data DocTest = DocTest
    { testModule :: Text
    , testValue  :: Text
    , testInput  :: Text
    , testOutput :: Text
    }


runDocTest :: DocTest -> ElmRepl -> IO Result
runDocTest docTest repl =
    eval ("import " <> testModule docTest <> " exposing (..)") repl >>= \case
        Left  err -> pure (ReplError err)
        Right _   -> eval (testInput docTest) repl >>= \case
            Left  err    -> pure (ReplError err)
            Right output -> do
                let want, got :: Text
                    want = testOutput docTest
                    got  = removeANSICodes output
                if want /= got then pure (OutputMismatch got) else pure AllGood


data Result
    = AllGood
    | OutputMismatch Text
    | ReplError Text


extractDocTests :: [ElmModuleDoc] -> [DocTest]
extractDocTests = concatMap extractModuleDocTests


extractModuleDocTests :: ElmModuleDoc -> [DocTest]
extractModuleDocTests moduleDoc = concatMap
    (extractValueDocTests (moduleName moduleDoc))
    (moduleValues moduleDoc)


extractValueDocTests :: Text -> ElmValueDoc -> [DocTest]
extractValueDocTests testModule valueDoc =
    commentToDocTests testModule (valueName valueDoc) (valueComment valueDoc)


commentToDocTests :: Text -> Text -> Text -> [DocTest]
commentToDocTests testModule testValue comment = go
    []
    (filterCode $ Text.lines comment)
  where
    filterCode :: [Text] -> [Text]
    filterCode = Maybe.mapMaybe
        $ \line -> if isCode line then Just (Text.strip line) else Nothing

    isCode :: Text -> Bool
    isCode = Text.isPrefixOf "    "

    go :: [DocTest] -> [Text] -> [DocTest]
    go accum []       = accum
    go accum (l : ls) = case Text.uncons l of
        Just ('>', testInput) -> case ls of
            (testOutput : remaining) ->
                go (tidyDocTest DocTest {..} : accum) remaining

            _ -> go accum ls
        _ -> go accum ls

    tidyDocTest :: DocTest -> DocTest
    tidyDocTest docTest = docTest
        { testInput  = Text.strip (testInput docTest)
        , testOutput = Text.strip (testOutput docTest)
        }


showText :: Show a => a -> Text
showText = Text.pack . show
