module ClassParser where

import           Common      (Parser)
import           ItemParser  (Item)
import qualified Text.Parsec as Parsec

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
parseClass :: Parser () (Class a)
parseClass = _

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
