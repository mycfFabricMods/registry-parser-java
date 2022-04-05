{-# OPTIONS_GHC -Wno-name-shadowing #-}
module ItemParser where

import           Common                (Parser, Registerable (..))
import           Data.Functor          ((<&>))
import           Data.Functor.Identity (Identity)
import           Text.Parsec           (ParsecT)
import qualified Text.Parsec           as Parsec
import           Text.Read             (readMaybe)

data Item = Itm
            { itemName     :: String
            , itemClass    :: ItemClass
            , itemSettings :: String
            } deriving (Show, Eq)

instance Registerable Item where
  parseRegister = fullItemParser

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

-- | parses a single Item type
parseItem :: Parser u String
parseItem = Parsec.string "Item"

parseBlock :: Parser u String
parseBlock = Parsec.string "Block"

-- | parses the new keyword
parseNew :: Parser u String
parseNew = Parsec.string "new"

-- | Parse the "public static final Item" part
-- or "public static final Block" part
parsePSFI :: Parser u ()
parsePSFI = do
    Parsec.spaces
    _ <- parseP
    Parsec.spaces
    _ <- parseS
    Parsec.spaces
    _ <- parseF
    Parsec.spaces
    _ <- parseItem Parsec.<|> parseBlock
    Parsec.spaces

-- | Parse the "public static final Item" and extracts the Item name, parses until = (inclusive)
parseItemName :: ParsecT String () Identity String
parseItemName = do
    _ <- parsePSFI
    Parsec.spaces
    itemName <- Parsec.many1 (Parsec.letter Parsec.<|> Parsec.char '_')
    Parsec.spaces
    _ <- Parsec.oneOf "="
    Parsec.optional Parsec.spaces
    pure itemName

data ItemClass
    = Item
    | BlockItem
    | AliasedBlockItem
    | Other String
    deriving (Show, Eq, Read)

-- | This parses the item type/class such as Item or AliasedBlockItem
parseItemType :: ParsecT String () Identity ItemClass
parseItemType = do
    Parsec.spaces
    -- first checks if it can be read, if yes then return the value inside the maybe
    -- otherwise convert it to a Maybe (Other String) and return the Other String
    Parsec.many1 Parsec.letter <&> \x -> case readMaybe x of
        Just x' -> x'
        Nothing -> Other x


-- | parses the item settings and if applicable the block
-- Item needs to parse the following:
-- new Item(new Item.Settings().group(ItemGroup.FOOD).food(MidasFoodComponents.COOKED_GOLDEN_PORKCHOP));
-- disregarding the first (, the last ) and the ;
-- AliasedBlockItem and BlockItem needs to parse the following:
-- new AliasedBlockItem(MidasBlocks.SWEET_GOLDEN_BERRY_BUSH, new Item.Settings().group(ItemGroup.FOOD).food(MidasFoodComponents.SWEET_GOLDEN_BERRIES));
-- disregarding the first (, the last ) and the ; and extracting the block
parseItemSettings :: Parser () String
parseItemSettings = do
    _ <- Parsec.oneOf "("
    Parsec.manyTill Parsec.anyChar (Parsec.oneOf ";") <&> init

fullItemParser :: Parser () Item
fullItemParser = do
    itemName <- parseItemName
    _ <- parseNew
    itemClass <- parseItemType
    Itm itemName itemClass <$> parseItemSettings
