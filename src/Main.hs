{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import Prelude

import qualified Control.Exception as Exception
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (readFile)
import qualified Language.Elm as Elm
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified Test.DocTest as DocTest
import qualified Text.Pretty as Pretty

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Test.DocTest (DocTest)
import Text.Pretty (Pretty)


main :: IO ()
main = do
    -- TODO: check we're in an elm project etc
    -- TODO: run tests concurrently
    (errors, passes) <- createDocs >>= testDocs
    printPassCount (length passes)
    printErrorCount (length errors)
    putChar '\n'
    traverse_ printError errors
    if null errors then Exit.exitFailure else Exit.exitSuccess
  where
    createDocs :: IO Elm.Docs
    createDocs =
        withTemporaryFile "docs.json"
            $   Elm.makeDocs
            >=> either (crashWith . makeDocsError) pure

    testDocs :: Elm.Docs -> IO ([Error], [()])
    testDocs docs =
        Either.partitionEithers <$> traverse runDocTest (extractDocTests docs)

    printPassCount :: Int -> IO ()
    printPassCount n =
        print . Pretty.styled [Pretty.green] $ showText n <> " passes"

    printErrorCount :: Int -> IO ()
    printErrorCount n =
        print . Pretty.styled [Pretty.red] $ showText n <> " errors"


runDocTest :: DocTest -> IO (Either Error ())
runDocTest docTest = do
    readResult <- readElmModule (DocTest.docTestModule docTest)
    case readResult of
        Left  err       -> pure (Left err)
        Right elmModule -> Elm.withRepl $ \repl -> do
            _ <- evalAll repl (initialImports (Elm.moduleImports elmModule))
            processResult <$> DocTest.run docTest repl
  where
    processResult :: DocTest.Result -> Either Error ()
    processResult = \case
        DocTest.AllGood               -> Right ()

        DocTest.OutputMismatch got    -> Left (outputMismatchError docTest got)

        DocTest.ReplError      stderr -> Left (replError docTest stderr)

    evalAll :: Elm.Repl -> [Text] -> IO (Either Text [Text])
    evalAll repl exprs = sequence <$> traverse (Elm.eval repl) exprs

    initialImports :: [Elm.Import] -> [Text]
    initialImports moduleImports =
        ("import " <> DocTest.docTestModule docTest <> " exposing (..)")
            : fmap Elm.printSource moduleImports


-- ERRORS


newtype Error = Error { unwrapError :: [Pretty] }


crashWith :: Error -> IO a
crashWith err = printError err *> Exit.exitFailure


printError :: Error -> IO ()
printError err = do
    putStrLn . concatMap show . unwrapError $ err
    putChar '\n'


makeDocsError :: Elm.MakeDocsError -> Error
makeDocsError (Elm.MakeDocsExitError     code) = makeDocsExitError code
makeDocsError (Elm.MakeDocsDecodingError why ) = makeDocsDecodingError why


makeDocsExitError :: Int -> Error
makeDocsExitError _code = Error [Pretty.plain "error decoding docs.json"]


makeDocsDecodingError :: String -> Error
makeDocsDecodingError _why = Error [Pretty.plain "error decoding docs.json"]

parseError :: Text -> Error
parseError why = Error [Pretty.plain why]


outputMismatchError :: DocTest -> Text -> Error
outputMismatchError docTest got = Error
    [ Pretty.styled
        [Pretty.cyan]
        (DocTest.docTestModule docTest <> "." <> DocTest.docTestValue docTest)
    , Pretty.plain "\n"
    , Pretty.plain "Your example says "
    , Pretty.styled [Pretty.yellow] (DocTest.docTestInput docTest)
    , Pretty.plain " should give   "
    , Pretty.styled [Pretty.yellow] (DocTest.docTestOutput docTest)
    , Pretty.plain "\n"
    , Pretty.plain "But when I ran it "
    , Pretty.styled [Pretty.yellow] (DocTest.docTestInput docTest)
    , Pretty.plain " actually gave "
    , Pretty.styled [Pretty.yellow] got
    , Pretty.plain "\n"
    ]


replError :: DocTest -> Text -> Error
replError docTest err = Error
    [ Pretty.styled
        [Pretty.cyan]
        (DocTest.docTestModule docTest <> "." <> DocTest.docTestValue docTest)
    , Pretty.plain "\n"
    , Pretty.plain err
    ]


readElmModule :: Text -> IO (Either Error Elm.Module)
readElmModule moduleName = do
    let splitModuleName = fmap Text.unpack (Text.splitOn "." moduleName)
        modulePath =
            FilePath.joinPath ("src" : splitModuleName)
                `FilePath.addExtension` "elm"
    first parseError . Elm.parseModule <$> Text.readFile modulePath


withTemporaryFile :: String -> (FilePath -> IO a) -> IO a
withTemporaryFile template action = do
    tmpDir <- Directory.getTemporaryDirectory
    Exception.bracket (before tmpDir) after (action . fst)
  where
    before :: FilePath -> IO (FilePath, IO.Handle)
    before tmpDir = do
        (filePath, handle) <- IO.openTempFile tmpDir template
        IO.hClose handle -- close straight away
        pure (filePath, handle)

    after :: (FilePath, IO.Handle) -> IO ()
    after (filePath, _) = attempt (Directory.removeFile filePath)


attempt :: IO () -> IO ()
attempt = Exception.handle swallowExceptions


swallowExceptions :: Exception.SomeException -> IO ()
swallowExceptions _ = pure ()


extractDocTests :: [Elm.ModuleDoc] -> [DocTest]
extractDocTests = concatMap extractModuleDocTests


extractModuleDocTests :: Elm.ModuleDoc -> [DocTest]
extractModuleDocTests moduleDoc = concatMap
    (extractValueDocTests (Elm.moduleDocName moduleDoc))
    (Elm.moduleDocValues moduleDoc)


extractValueDocTests :: Text -> Elm.ValueDoc -> [DocTest]
extractValueDocTests testModule valueDoc = commentToDocTests
    testModule
    (Elm.valueDocName valueDoc)
    (Elm.valueDocComment valueDoc)


commentToDocTests :: Text -> Text -> Text -> [DocTest]
commentToDocTests docTestModule docTestValue comment = go
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
        Just ('>', docTestInput) -> case ls of
            (docTestOutput : remaining) ->
                go (tidyDocTest DocTest.DocTest {..} : accum) remaining

            _ -> go accum ls
        _ -> go accum ls

    tidyDocTest :: DocTest -> DocTest
    tidyDocTest docTest = docTest
        { DocTest.docTestInput  = Text.strip (DocTest.docTestInput docTest)
        , DocTest.docTestOutput = Text.strip (DocTest.docTestOutput docTest)
        }


showText :: Show a => a -> Text
showText = Text.pack . show
