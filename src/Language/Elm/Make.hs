{-# LANGUAGE LambdaCase #-}
module Language.Elm.Make
    ( MakeDocsError
        ( MakeDocsExitError
        , MakeDocsDecodingError
        )
    , makeDocs
    )
where

import Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Process as Process

import Data.Bifunctor (first)
import Language.Elm.Doc (Docs)


data MakeDocsError
    = MakeDocsExitError Int
    | MakeDocsDecodingError String


makeDocs :: FilePath -> IO (Either MakeDocsError Docs)
makeDocs filePath = do
    (_, _, _, processHandle) <- Process.createProcess
        (Process.proc "elm" ["make", "--docs", filePath])
            { Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }
    Process.waitForProcess processHandle >>= \case
        Exit.ExitFailure code -> pure (Left (MakeDocsExitError code))
        Exit.ExitSuccess -> IO.withFile filePath IO.ReadMode $ \handle -> do
            contents <- ByteString.hGetContents handle
            pure $ first MakeDocsDecodingError
                         (Aeson.eitherDecodeStrict contents)
