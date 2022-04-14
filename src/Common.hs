module Common where

import           Data.Functor.Identity (Identity)
import           Text.Parsec           (ParsecT)
import qualified Text.Parsec           as Parsec

type Parser u a = ParsecT String u Identity a

data RegistryType
    = RItem
    | RBlock
    | ROther String
    deriving (Eq, Show)

data Identifier = Identifier
    { identifierForMod :: String
    , identifierName   :: String
    } deriving (Eq, Ord, Show)

-- | Shortcut for parsing a string with Parsec
parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse = flip Parsec.parse "(source)"

-- | parses a single public keyword
parseP :: Parser u String
parseP = Parsec.string "public"

-- | parses a single private keyword
parsePri :: Parser u String
parsePri = Parsec.string "private"

-- | parses a single static keyword
parseS :: Parser u String
parseS = Parsec.string "static"

-- | parses a single final keyword
parseF :: Parser u String
parseF = Parsec.string "final"

-- | parses a single abstract keyword
parseA :: Parser u String
parseA = Parsec.string "abstract"

parseModifier :: Parser u String
parseModifier = parseP Parsec.<|> parsePri Parsec.<|> parseS Parsec.<|> parseF Parsec.<|> parseA

-- | parses the new keyword
parseNew :: Parser u String
parseNew = Parsec.string "new"

-- Dummy class for blocks and items
class Registerable a where
  parseRegister :: Parser () a
