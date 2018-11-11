{-# LANGUAGE InstanceSigs #-}
module Text.Pretty
    ( Pretty
    , styled
    , plain
    , scrub
    , Style
    , black
    , red
    , green
    , yellow
    , blue
    , magenta
    , cyan
    , white
    , bold
    , underlined
    )
where

import Prelude

import qualified Data.Text as Text
import qualified System.Console.ANSI as ANSI

import Data.Text (Text)


data Pretty
    = Styled [Style] Text
    | Plain Text


instance Show Pretty where
    show :: Pretty -> String
    show = render


styled :: [Style] -> Text -> Pretty
styled = Styled


plain :: Text -> Pretty
plain = Plain


-- | Remove styles.
scrub :: Pretty -> Text
scrub (Styled _ text) = text
scrub (Plain text   ) = text


render :: Pretty -> String
render (Plain text        ) = Text.unpack text
render (Styled styles text) = mconcat
    [ ANSI.setSGRCode (map styleToSGR styles)
    , Text.unpack text
    , ANSI.setSGRCode [ANSI.Reset]
    ]


data Style
    = Color ANSI.Color
    | Underlined
    | Bold


black :: Style
black = Color ANSI.Black


red :: Style
red = Color ANSI.Red


green :: Style
green = Color ANSI.Green


yellow :: Style
yellow = Color ANSI.Yellow


blue :: Style
blue = Color ANSI.Blue


magenta :: Style
magenta = Color ANSI.Magenta


cyan :: Style
cyan = Color ANSI.Cyan


white :: Style
white = Color ANSI.White


underlined :: Style
underlined = Underlined


bold :: Style
bold = Bold


styleToSGR :: Style -> ANSI.SGR
styleToSGR Underlined    = ANSI.SetItalicized True
styleToSGR Bold          = ANSI.SetConsoleIntensity ANSI.BoldIntensity
styleToSGR (Color color) = ANSI.SetColor ANSI.Foreground ANSI.Vivid color
