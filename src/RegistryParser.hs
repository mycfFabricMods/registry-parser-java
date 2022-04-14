module RegistryParser where

import           Common       (Identifier (..), Parser, RegistryType (..),
                               parseNew)
import           Data.Functor (($>), (<&>))
import qualified Text.Parsec  as Parsec

data Registry a = Registry RegistryType Identifier String
    deriving (Eq, Show)

-- | parses the Registry.register( part of the registry, discarding the values
registryDotRegisterParser :: Parser () ()
registryDotRegisterParser = Parsec.manyTill Parsec.anyChar (Parsec.char '(') $> ()


-- | Parses the registry 'Registry.{ITEM, Block, Entity, ...}'  part and returns the registry type
-- Returns Item if the registry is an item registry
-- Returns Block if the registry is a block registry
-- Returns Other if the registry is neither an item nor a block registry
registryTypeParser :: Parser () RegistryType
registryTypeParser = Parsec.string "Registry."
                     >> Parsec.manyTill Parsec.letter (Parsec.char ',')
                     <&> \case
                           "ITEM"  -> RItem
                           "BLOCK" -> RBlock
                           x       -> ROther x

-- | errors on the between line, its not getting the closing thing
identifierParser :: Parser () Identifier
identifierParser = do
    Parsec.spaces
    _ <- parseNew
    Parsec.spaces
    _ <- Parsec.string "Identifier("
    Parsec.spaces
    for  <- Parsec.manyTill Parsec.anyChar (Parsec.char ',')
    Parsec.spaces
    -- I had errors with between but I don't know why
    _ <- Parsec.char '"'
    name <- Parsec.manyTill Parsec.anyChar (Parsec.char '"')
    Parsec.spaces
    _ <- Parsec.char ')'
    Parsec.spaces
    _ <- Parsec.char ','
    Parsec.spaces
    pure $ Identifier for name

registryItemParser :: Parser () String
registryItemParser = do
    _ <- Parsec.optional parseNew
    Parsec.spaces
    Parsec.manyTill Parsec.anyChar (Parsec.char ';') <&> init

registryParser :: Parser () (Registry String)
registryParser = do
    registryDotRegisterParser
    Parsec.spaces
    registryType <- registryTypeParser
    Parsec.spaces
    identifier <- identifierParser
    Parsec.spaces
    Registry registryType identifier <$> registryItemParser

