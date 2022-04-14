module ClassParser where

import           Common       (Parser, Registerable, parseModifier)
import           Data.Functor ((<&>))
import           ItemParser   (Item)
import qualified Text.Parsec  as Parsec

-- not implemented yet
data Block = MockBlock

-- The package in which the class lies
type Package = String

data Class a where
    ItemClass  :: Package -> String -> [Item]  -> Class Item
    BlockClass :: Package -> String -> [Block] -> Class Block
    OtherClass :: Package -> String -> [a]     -> Class a

-- | parses a whole class and returns a Class
-- the returned Class can be used to rewrite the file
parseClass :: Registerable a => Parser () (Class a)
parseClass = undefined

-- | parses the beginning of a java class file
-- extracting the package and the class name
parseUntilClassContent :: Parser () (Package, String)
parseUntilClassContent = do
    pkg  <- parsePackage
    _    <- parseImports
    name <- parseClassName
    pure (pkg, name)
    where
        parseImports   = Parsec.manyTill Parsec.anyChar (Parsec.string "class")
        parseClassName = Parsec.manyTill Parsec.letter (Parsec.char '{')
        parsePackage   = Parsec.spaces >> Parsec.string "package" >> Parsec.spaces >> Parsec.manyTill Parsec.letter (Parsec.char ';')

-- | Data type to represent a method
data Method a = Method
  { methodName   :: String     -- ^ Name of the method
  , methodMods   :: String     -- ^ Modifiers of the method
  , methodReturn :: String     -- ^ Return type of the method
  , methodArgs   :: String     -- ^ Parameters of the method
  , methodData   :: a          -- ^ Method body
  } deriving (Show, Eq)

simpleMethodParser :: Parser () (Method String)
simpleMethodParser = do
    modifiers <- parseModifier `Parsec.sepBy` Parsec.spaces <&> unwords
    ret  <- Parsec.many1 Parsec.letter
    name <- Parsec.manyTill Parsec.letter (Parsec.char '(')
    args <- Parsec.manyTill Parsec.letter (Parsec.char ')')
    dada <- Parsec.manyTill Parsec.anyChar (Parsec.char '}')
    pure Method { methodName = name
                , methodMods = modifiers
                , methodReturn = ret
                , methodArgs = args
                , methodData = dada
                }

