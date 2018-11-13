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
    , runWith
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
run = runWith id


runWith :: Interpreter a => (Text -> Text) -> DocTest -> a -> IO Result
runWith prepare docTest interpreter = do
    result <- eval interpreter (docTestInput docTest)
    case result of
        Left  err    -> pure (ReplError err)
        Right output -> do
            let want, got :: Text
                want = prepare (docTestOutput docTest)
                got  = prepare (output)

            if want == got then pure AllGood else pure (OutputMismatch got)
