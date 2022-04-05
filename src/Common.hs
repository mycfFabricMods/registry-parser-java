module Common where

import           Data.Functor.Identity (Identity)
import           Text.Parsec           (ParsecT)
import qualified Text.Parsec           as Parsec

type Parser u a = ParsecT String u Identity a

-- | Shortcut for parsing a string with Parsec
parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse = flip Parsec.parse "(source)"

-- Dummy class for blocks and items
class Registerable a where
  parseRegister :: Parser () a
