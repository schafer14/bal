import RIO

import qualified RIO.Map as Map
import qualified Ast 
import Parser (parser) 
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn, succeedsLeaving, initialState)
import Text.Megaparsec (parse, ParseErrorBundle, runParser')


main :: IO ()
main = hspec $ do
  describe "Ast.parse" $ do
    describe "parsing content" $ do
      it "parses text" $ do
        testParser "hello world" `shouldParse` [txt "hello world"]
        testParser "multi\nline \ntext" `shouldParse` [txt "multi line  text"]
        testParser "test multi newlines\n\n" `shouldParse` [txt "test multi newlines", nl]
        testParser "hello \\*world\\*" `shouldParse` [txt "hello *world*"]
        testParser "hello \\\\world\\\\" `shouldParse` [txt "hello \\world\\"]
        testParser "hello >" `shouldParse` [txt "hello >"]
        testParser "hello \\|>h1" `shouldParse` [txt "hello |>h1"]

      it "parses newlines" $ do
        testParser "test multi newlines\n\n\n\n" `shouldParse` [txt "test multi newlines", nl]
        testParser "multi\n\ncontent" `shouldParse` [txt "multi", nl, txt "content"]
        testParser "\nmulti" `shouldParse` [txt "multi"]

      it "parses bold text" $ do 
        testParser "some *bolded* test" `shouldParse` [txt "some ", bold [txtC "bolded"], txt " test"]
        testParser "*bold\nwith a newline*" `shouldParse` [bold [txtC "bold with a newline"]]
        testParser "*bold /with/ some italics*" `shouldParse` [bold [txtC "bold ", italicC [txtC "with"], txtC " some italics"]]

      it "parses italic text" $ do 
        testParser "/italic\nwith a newline/" `shouldParse` [italic [txtC "italic with a newline"]]
        testParser "text /with/ some italics" `shouldParse` [txt "text ", italic [txtC "with"], txt " some italics"]
        testParser "/with/ some italics" `shouldParse` [italic [txtC "with"], txt " some italics"]
        runParser' parser (initialState "/italic\n\nwith a newline/") `succeedsLeaving` ""
        testParser "/italic\n\nwith a newline/" `shouldParse` [italic $ [txtC "italic", Ast.NewLine, txtC "with a newline"]]

      it "parses blocks" $ do 
        testParser "|> H1\n \\attr1\n" `shouldParse` [block "H1" ["attr1"] []]
        testParser "|> H1\n \\attr1\n \\attr2\n" `shouldParse` [block "H1" ["attr1", "attr2"] []]
        testParser "|> H1\n  \\attr1\n  \\attr2\n" `shouldParse` [block "H1" ["attr1", "attr2"] []]
        parse parser "" `shouldFailOn` "|> H1\n  \\attr1\n   \\attr2\n"
        parse parser "" `shouldFailOn` "|> H1\n   \\attr1\n  \\attr2\n"
        testParser "|> H1\n  \\attr1\n  \\attr2\nsome content here" `shouldParse` [block "H1" ["attr1", "attr2"] [], txt "some content here"]
        testParser "Some content\ntest\n|> H1\n" `shouldParse` [txt "Some content test", block "H1" [] []]
        testParser "Some content\ntest\n|>more \ncontent" `shouldParse` [txt "Some content test", block "more" [] [], txt "content"]
        --
        testParser "Some content\ntest\n|>more\n content" `shouldParse` [txt "Some content test", block "more" [] [txt "content"]]
        runParser' parser (initialState "Some content\ntest\n") `succeedsLeaving` ""
        testParser "Some content\ntest\n|>more\n \\attr1\ncontent" `shouldParse` [txt "Some content test", block "more" ["attr1"] [], txt "content"]
        testParser "Some content\ntest\n|>more\n \\attr1\n \\attr2\n" `shouldParse` [txt "Some content test", block "more" ["attr1", "attr2"] []]
        testParser "Some content\ntest\n|>more\n \\attr1\n \\attr2\ncontent" `shouldParse` [txt "Some content test", block "more" ["attr1", "attr2"] [], txt "content"]
        testParser "Some content\ntest\n|>more\n \\attr1\n \\attr2\n content" `shouldParse` [txt "Some content test", block "more" ["attr1", "attr2"] [txt "content"]]

        testParser "Some content\ntest\n|>more\n \\attr1\n \\attr2\n content\nxyz" `shouldParse` [txt "Some content test", block "more" ["attr1", "attr2"] [txt "content"], txt "xyz"]
        testParser "Some content\ntest\n|>more\n \\attr1\n content\nlast" `shouldParse` [txt "Some content test", block "more" ["attr1"] [txt "content"], txt "last"]
        testParser "Some content\ntest\n|>more\n \\attr1\n *content*\nlast" `shouldParse` [txt "Some content test", block "more" ["attr1"] [bold [txtC "content"]], txt "last"]
        --
        testParser "Some content\ntest\n|>more\n \\attr1\n *content*" `shouldParse` [txt "Some content test", block "more" ["attr1"] [bold [txtC "content"]]]
        testParser "|> H1\n \\attr1\n|> H2\n" `shouldParse` [block "H1" ["attr1"] [], block "H2" [] []]
        --
        testParser "|> H1\n |> H2\n" `shouldParse` [block "H1" [] [block "H2" [] [] ]]
        testParser "|> H1\n|> H2\n" `shouldParse` [block "H1" [] [], block "H2" [] []]
        runParser' parser (initialState "|> H1\n|> H2\n") `succeedsLeaving` ""
        --
        runParser' parser (initialState "|> H1\n \\attr1\n") `succeedsLeaving` ""
        runParser' parser (initialState "|> H1\n") `succeedsLeaving` ""
        runParser' parser (initialState "|> H1\n |> H2\n") `succeedsLeaving` ""

        testParser "|> H1\n|> H2\n|> H3\n" `shouldParse` [block "H1" [] [], block "H2" [] [], block "H3" [] []]
        testParser "|> H1\n |> H2\n  |> H3\n" `shouldParse` [block "H1" [] [ block "H2" [] [block "H3" [] [] ] ]]
        testParser "|> H1\n |> H2\n  |> H3\nhey there" `shouldParse` [block "H1" [] [ block "H2" [] [block "H3" [] [] ] ], txt "hey there"]
        testParser "|> H1\n |> H2\n  |> H3\n hey there" `shouldParse` [block "H1" [] [ block "H2" [] [block "H3" [] [] ], txt "hey there" ]]
        testParser "|> H1\n |> H2\n  |> H3\n  hey there" `shouldParse` [block "H1" [] [ block "H2" [] [block "H3" [] [], txt "hey there"] ]]
        testParser "|> H1\n |> H2\n  |> H3\n   hey there" `shouldParse` [block "H1" [] [ block "H2" [] [block "H3" [] [ txt "hey there"]] ]]
        testParser "\n\n|> H1\n |> H2\n  |> H3\n   hey there" `shouldParse` [block "H1" [] [ block "H2" [] [block "H3" [] [ txt "hey there"]] ]]
        testParser "  \n\n  |> H1\n |> H2\n  |> H3\n   hey there" `shouldParse` [block "H1" [] [ block "H2" [] [block "H3" [] [ txt "hey there"]] ]]

      it "parses dirctives" $ do 
        testParser "[some content]{ link | url = website.com }" `shouldParse` [dir "link" [("url", "website.com")] (txtC "some content")]
        testParser "[xyz]{ link | url = website.com, label = thing }" `shouldParse` [dir "link" [("url", "website.com"), ("label", "thing")] (txtC "xyz")]



-- HELPERS
txt :: Text -> Ast.Expression
txt t = Ast.ExprContent $ Ast.Content $ Ast.TextContent t

txtC :: Text -> Ast.Content
txtC t =  Ast.Content $ Ast.TextContent t

nl :: Ast.Expression
nl = Ast.ExprContent Ast.NewLine

bold :: [Ast.Content] -> Ast.Expression
bold t = Ast.ExprContent $ Ast.Content $ Ast.DecoratedText Ast.Bold t

italic :: [Ast.Content] -> Ast.Expression
italic t = Ast.ExprContent $ Ast.Content $ Ast.DecoratedText Ast.Italic t

italicC :: [Ast.Content] -> Ast.Content
italicC t = Ast.Content $ Ast.DecoratedText Ast.Italic t

block :: Text -> [Text] -> [Ast.Expression] -> Ast.Expression
block name attrs children = Ast.ExprBlock $ Ast.Block name attrs children

dir :: Text -> [(Text, Text)] -> Ast.Content -> Ast.Expression
dir name attrs children = Ast.ExprContent $ Ast.Directive name (Map.fromList attrs) children

testParser :: Text -> Either (ParseErrorBundle Text Void) [Ast.Expression]
testParser t = parse parser "test.bal" t


