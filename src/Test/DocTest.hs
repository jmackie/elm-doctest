module Test.DocTest
    ( DocTest
        ( DocTest
        , docTestModule
        , docTestValue
        , docTestInput
        , docTestOutput
        )
    , Result
        ( AllGood
        , OutputMismatch
        , ReplError
        )
    , run
    )
where

import Prelude

import Data.Interpreter (Interpreter, eval)
import Data.Text (Text)


data DocTest = DocTest
    { docTestModule :: Text  -- ^ Module where the test is coming from
    , docTestValue  :: Text  -- ^ Subject of the doctest
    , docTestInput  :: Text  -- ^ Input
    , docTestOutput :: Text  -- ^ Expected output
    }


data Result
    = AllGood
    | OutputMismatch Text
    | ReplError Text


run :: Interpreter a => DocTest -> a -> IO Result
run docTest interpreter = do
    result <- eval interpreter (docTestInput docTest)
    case result of
        Left  err    -> pure (ReplError err)
        Right output -> do
            let want, got :: Text
                want = docTestOutput docTest
                got  = output

            if want /= got then pure (OutputMismatch got) else pure AllGood
