module Data.Interpreter
    ( Interpreter(eval)
    )
where

import Prelude

import Data.Text (Text)


class Interpreter a where
    eval :: a -> Text -> IO (Either Text Text)
