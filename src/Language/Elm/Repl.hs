{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Elm.Repl
    ( Repl
    , withRepl
    , Data.Interpreter.eval
    )
where

import Prelude

import qualified Control.Exception as Exception
import qualified Data.Attoparsec.Text as Parse
import qualified Data.Text as Text
import qualified System.IO as IO
import qualified System.Process as Process

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Interpreter (Interpreter(eval))
import Data.Text (Text)


data Repl = Repl
    { processHandle :: Process.ProcessHandle
    , stdin         :: IO.Handle
    , stdout        :: IO.Handle
    , stderr        :: IO.Handle
    }


instance Interpreter Repl where
    eval ::  Repl -> Text -> IO (Either Text Text)
    eval = eval'


withRepl :: (Repl -> IO a) -> IO a
withRepl = Exception.bracket runRepl closeRepl


runRepl :: IO Repl
runRepl = do
    (Just stdin, Just stdout, Just stderr, processHandle) <-
        Process.createProcess $ (Process.proc "elm" ["repl"])
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }
    let repl = Repl {..}
    -- Consume the repl header
    _ <- getOutput repl
    pure repl


closeRepl :: Repl -> IO ()
closeRepl repl = do
    -- NOTE: order here is important!
    IO.hClose (stdin repl)
    _ <- Process.waitForProcess (processHandle repl)
    IO.hClose (stdout repl)


eval' :: Repl -> Text -> IO (Either Text Text)
eval' repl expr = do
    putExpr (Text.unpack expr) repl
    output   <- getOutput repl
    maybeErr <- getError repl
    case maybeErr of
        Nothing  -> pure . Right . removeANSICodes . Text.pack $ output
        Just err -> pure . Left . removeANSICodes . Text.pack $ err


putExpr :: String -> Repl -> IO ()
putExpr expr repl = do
    IO.hPutStrLn (stdin repl) expr
    IO.hFlush (stdin repl)


getOutput :: Repl -> IO String
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


getError :: Repl -> IO (Maybe String)
getError repl = do
    errored <- IO.hReady (stderr repl)
    if errored then Just <$> IO.hGetContents (stderr repl) else pure Nothing


removeANSICodes :: Text -> Text
removeANSICodes text = case Parse.parseOnly parser (filterEscapes text) of
    Left  _        -> text
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
