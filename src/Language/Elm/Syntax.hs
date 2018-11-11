{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Elm.Syntax
    ( Module
        ( Module
        , moduleName
        , moduleExposing
        , moduleImports
        )
    , Import
        ( Import
        , importModuleName
        , importAlias
        , importExposing
        )
    , parseModule
    , printSource
    )
where

import Prelude

import qualified Data.Attoparsec.Text as Parse
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)


class Source a where
    printSource :: a -> Text


-- | There's obviously more to an Elm module but this is all I care about for now.
data Module = Module
    { moduleName     :: ModuleName
    , moduleExposing :: Exposing
    , moduleImports  :: [Import]
    } deriving Show


newtype LowerName =
    LowerName { unwrapLowerName :: Text } deriving Show


instance Source LowerName where
    printSource = unwrapLowerName


newtype UpperName =
    UpperName { unwrapUpperName :: Text } deriving Show


instance Source UpperName where
    printSource = unwrapUpperName


newtype ModuleName =
    ModuleName { unwrapModuleName :: NonEmpty UpperName } deriving Show


instance Source ModuleName where
    printSource = Text.intercalate "."
                . fmap printSource
                . NonEmpty.toList
                . unwrapModuleName


data Import = Import
    { importModuleName :: ModuleName
    , importAlias      :: Maybe UpperName
    , importExposing   :: Maybe Exposing
    } deriving Show


instance Source Import where
    printSource Import {..} = Text.intercalate " " . Maybe.catMaybes $
        [ Just "import"
        , Just (printSource importModuleName )
        , ("as "<>) . printSource <$> importAlias
        , ("exposing "<>) . printSource <$> importExposing
        ]


data Exposing
    = ExposingAll
    | ExposingOnly (NonEmpty Exposed)
    deriving Show


instance Source Exposing where
    printSource ExposingAll = "(..)"
    printSource (ExposingOnly exposed) =
        "(" <>
        Text.intercalate ", " (fmap printSource (NonEmpty.toList exposed)) <>
        ")"


data Exposed
    = ExposedValue LowerName
    | ExposedType UpperName Privacy
    deriving Show


instance Source Exposed where
    printSource (ExposedValue name)        = printSource name
    printSource (ExposedType name Public)  = printSource name <> "(..)"
    printSource (ExposedType name Private) = printSource name


data Privacy
    = Public
    | Private
    deriving Show


-- SYNTAX PARSING


parseModule :: Text -> Either Text Module
parseModule = first Text.pack . Parse.parseOnly parseModule'


parseModule' :: Parse.Parser Module
parseModule' = do
    _              <- parseToken (parseKeyword "module")
    moduleName     <- parseToken parseModuleName
    moduleExposing <- parseToken (parseKeyword "exposing")
        *> parseToken parseExposing
    moduleImports <- Parse.many' parseImport
    pure Module {..}


parseImport :: Parse.Parser Import
parseImport = do
    _                <- parseToken (parseKeyword "import")
    importModuleName <- parseToken parseModuleName
    importAlias      <- parseOptional
        (parseToken (parseKeyword "as") *> parseToken parseUpperName)
    importExposing <- parseOptional
        (parseToken (parseKeyword "exposing") *> parseToken parseExposing)
    pure Import {..}


parseExposing :: Parse.Parser Exposing
parseExposing = parseExposingAll <|> parseExposingOnly
  where
    parseExposingAll :: Parse.Parser Exposing
    parseExposingAll = Parse.string "(..)" $> ExposingAll

    parseExposingOnly :: Parse.Parser Exposing
    parseExposingOnly = do
        _       <- parseToken (Parse.char '(')
        exposed <- parseToken parseExposed
            `Parse.sepBy1` parseToken (Parse.char ',')
        _ <- parseToken (Parse.char ')')
        pure (ExposingOnly (NonEmpty.fromList exposed))


parseExposed :: Parse.Parser Exposed
parseExposed = parseExposedValue <|> parseExposedType
  where
    parseExposedValue :: Parse.Parser Exposed
    parseExposedValue = ExposedValue <$> parseLowerName

    parseExposedType :: Parse.Parser Exposed
    parseExposedType = ExposedType <$> parseUpperName <*> parsePrivacy


parsePrivacy :: Parse.Parser Privacy
parsePrivacy = (Parse.string "(..)" $> Public) <|> pure Private


parseModuleName :: Parse.Parser ModuleName
parseModuleName =
    ModuleName
        .   NonEmpty.fromList
        <$> (parseUpperName `Parse.sepBy1` Parse.char '.')


parseLowerName :: Parse.Parser LowerName
parseLowerName = LowerName <$> parseIdent Char.isLower


parseUpperName :: Parse.Parser UpperName
parseUpperName = UpperName <$> parseIdent Char.isUpper


parseIdent :: (Char -> Bool) -> Parse.Parser Text
parseIdent p = Text.cons <$> Parse.satisfy p <*> Parse.takeWhile isIdentChar
  where
    isIdentChar :: Char -> Bool
    isIdentChar c = Char.isAlphaNum c || c == '_'


parseKeyword :: Text -> Parse.Parser ()
parseKeyword = void . Parse.string


-- TOKEN PARSING


parseToken :: Parse.Parser a -> Parse.Parser a
parseToken = (<* skipSpaceAndComments)


skipSpaceAndComments :: Parse.Parser ()
skipSpaceAndComments = Parse.choice
    [ skipSpace *> skipSpaceAndComments
    , skipLineComments *> skipSpaceAndComments
    , skipMultilineComments *> skipSpaceAndComments
    , pure ()
    ]


skipSpace :: Parse.Parser ()
skipSpace = Parse.many1' Parse.space $> ()


skipLineComments :: Parse.Parser ()
skipLineComments = Parse.string "--" *> Parse.takeTill Parse.isEndOfLine $> ()


skipMultilineComments :: Parse.Parser ()
skipMultilineComments = Parse.string "{-" *> go
  where
    go :: Parse.Parser ()
    go = do
        parsed <- Parse.takeTill (== '}')
        if "-" `Text.isSuffixOf` parsed then Parse.char '}' $> () else go


-- BASIC PARSERS


parseOptional :: Parse.Parser a -> Parse.Parser (Maybe a)
parseOptional parser = Parse.option Nothing (Just <$> parser)
