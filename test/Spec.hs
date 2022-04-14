
import           Common
import           ItemParser
import           RegistryParser
import           Test.Hspec     (describe, hspec, it, shouldBe)

main :: IO ()
main = do
    testItemTypeParser
    testItemSettingsParser
    testFullItemParser

    fullRegistryParser

testItemTypeParser :: IO ()
testItemTypeParser = hspec do
        describe "Testing the ItemTypeParser" do
            it "Item should return Item" do
                parse parseItemType "Item" `shouldBe` Right Item
            it "AliasedBlockItem should return AliasedBlockItem" do
                parse parseItemType "AliasedBlockItem" `shouldBe` Right AliasedBlockItem
            it "BlockItem should return BlockItem" do
                parse parseItemType "BlockItem" `shouldBe` Right BlockItem
            it "Block should return Other Block" do
                parse parseItemType "Block" `shouldBe` Right (Other "Block")
            it "FooBar should return Other FooBar" do
                parse parseItemType "FooBar" `shouldBe` Right (Other "FooBar")
            it "'    FooBar' should return Other FooBar" do
                parse parseItemType "     FooBar" `shouldBe` Right (Other "FooBar")
            it "'Foo Bar' should return Foo" do
                parse parseItemType "Foo Boor" `shouldBe` Right (Other "Foo")

testItemSettingsParser :: IO ()
testItemSettingsParser = hspec do
    describe "Tests if the parseItemSettings parses correctly" do
        it "(new Item.Settings().group(ItemGroup.FOOD).food(MidasFoodComponents.DRIED_GOLDEN_KELP));" do
            parse parseItemSettings "(new Item.Settings().group(ItemGroup.FOOD).food(MidasFoodComponents.DRIED_GOLDEN_KELP));" `shouldBe` Right "new Item.Settings().group(ItemGroup.FOOD).food(MidasFoodComponents.DRIED_GOLDEN_KELP)"
        it "(MidasBlocks.GOLDEN_BEETROOTS, new Item.Settings().group(ItemGroup.MATERIALS));" do
            parse parseItemSettings "(MidasBlocks.GOLDEN_BEETROOTS, new Item.Settings().group(ItemGroup.MATERIALS));" `shouldBe` Right "MidasBlocks.GOLDEN_BEETROOTS, new Item.Settings().group(ItemGroup.MATERIALS)"

testFullItemParser :: IO ()
testFullItemParser = hspec do
    describe "Tests if a full instantiated Item can be parsed correctly" do
        it "A normal Item" do
            parse fullItemParser "public static final Item DRIED_GOLDEN_KELP = new Item(new Item.Settings().group(ItemGroup.FOOD).food(MidasFoodComponents.DRIED_GOLDEN_KELP));"
                `shouldBe` Right (Itm "DRIED_GOLDEN_KELP" Item "new Item.Settings().group(ItemGroup.FOOD).food(MidasFoodComponents.DRIED_GOLDEN_KELP)")
        it "An AliasedBlockItem  Item" do
            parse fullItemParser "public static final Item GOLDEN_BEETROOT_SEEDS = new AliasedBlockItem(MidasBlocks.GOLDEN_BEETROOTS, new Item.Settings().group(ItemGroup.MATERIALS));"
                `shouldBe` Right (Itm "GOLDEN_BEETROOT_SEEDS" AliasedBlockItem "MidasBlocks.GOLDEN_BEETROOTS, new Item.Settings().group(ItemGroup.MATERIALS)")
        it "A StewItem Item" do
            parse fullItemParser "public static final Item GOLDEN_BEETROOT_SOUP = new StewItem(new Item.Settings().maxCount(1).group(ItemGroup.FOOD).food(MidasFoodComponents.GOLDEN_BEETROOT_SOUP));"
                `shouldBe` Right (Itm "GOLDEN_BEETROOT_SOUP" (Other "StewItem") "new Item.Settings().maxCount(1).group(ItemGroup.FOOD).food(MidasFoodComponents.GOLDEN_BEETROOT_SOUP)")

fullRegistryParser :: IO ()
fullRegistryParser = hspec do
    describe "Tests if a full registered Item can be parsed correctly" do
        it "A normal Item" do
            parse registryParser "Registry.register(Registry.ITEM, new Identifier(MidasHunger.MOD_ID, \"golden_turtle_egg\"), GOLDEN_TURTLE_EGG);"
                `shouldBe` Right (Registry RItem (Identifier "MidasHunger.MOD_ID" "golden_turtle_egg") "GOLDEN_TURTLE_EGG")
        it "A BlockItem" do
            parse registryParser "Registry.register(Registry.ITEM, new Identifier(MidasHunger.MOD_ID, \"golden_kelp\"), new BlockItem(MidasBlocks.GOLDEN_KELP, new Item.Settings().group(ItemGroup.DECORATIONS)));"
                `shouldBe` Right (Registry RItem (Identifier "MidasHunger.MOD_ID" "golden_kelp") "BlockItem(MidasBlocks.GOLDEN_KELP, new Item.Settings().group(ItemGroup.DECORATIONS))")

-- Registry.register(Registry.ITEM, new Identifier(MidasHunger.MOD_ID, \"golden_kelp\"), new BlockItem(MidasBlocks.GOLDEN_KELP, new Item.Settings().group(ItemGroup.DECORATIONS)));
