module Parser (parser) where

import RIO
import Ast
import RIO.Char (isSpace, isPrint)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as CharParser
import qualified RIO.Text as T
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = P.Parsec Void Text

parser :: Parser Ast
parser = sc >> p 
  where 
    expr = (P.try $ ExprContent <$> contentParser P.pos1) <|> (P.try $ parseBlock P.pos1)
    p = P.some expr

parseExprAt :: P.Pos -> Parser Expression
parseExprAt pos = p
  where
    p = do
      _ <- L.indentGuard sc EQ pos
      (P.try $ ExprContent <$> contentParser pos) <|> (P.try $ parseBlock pos)

sc :: Parser ()
sc = 
  L.space 
    CharParser.space1 
    (L.skipLineComment "//") 
    (L.skipBlockComment "/*" "*/")

hsc :: Parser ()
hsc = 
  L.space 
    CharParser.hspace1 
    (L.skipLineComment "//") 
    (L.skipBlockComment "/*" "*/")

contentParser :: P.Pos -> Parser Content
contentParser pos = p <?> "content"
  where 
    p 
      =   (P.try newLineParser)
      <|> (P.try $ parseItalic pos)
      <|> (P.try $ parseBold pos)
      <|> (P.try $ parseDirective pos)
      <|> (P.try $ textParser pos)

parseBlock :: P.Pos -> Parser Expression
parseBlock base = p <?> "block"
  where 
    p = do 
      _ <- CharParser.char '|'
      _ <- CharParser.char '>'
      hsc
      label <- P.some CharParser.alphaNumChar
      lvl <- sc >> L.indentLevel 
      if lvl <= base 
        then pure $ ExprBlock $ Block (T.pack label) [] []
        else do 
          attrs <- readAttrs lvl
          exprs <- P.many $ parseExprAt lvl 
          pure $ ExprBlock $ Block (T.pack label) attrs exprs
    
    readAttrs :: P.Pos -> Parser [Text]
    readAttrs pos = P.many $ P.try readAttr
      where
        readAttr = do 
          _ <- L.indentGuard sc EQ pos
          _ <- CharParser.char '\\'
          line <- P.someTill ((P.try CharParser.alphaNumChar) <|> (P.try spaceChar)) CharParser.newline
          pure $ T.pack line

    spaceChar :: Parser Char
    spaceChar = P.satisfy isHSpace <?> "horizontal space"

    isHSpace :: Char -> Bool
    isHSpace x = isSpace x && x /= '\n' && x /= '\r'
      

parseDirective :: P.Pos -> Parser Content
parseDirective pos = p
  where
    p = do 
      content <- P.between (CharParser.char '[') (CharParser.char ']') $ contentParser pos
      hsc
      (label, attrs) <- P.between (CharParser.char '{') (CharParser.char '}') $ dirAttrs
      _ <- P.optional CharParser.newline
      pure $ Directive label attrs content

    dirAttrs :: Parser (Text, Map Text Text)
    dirAttrs = do 
      hsc
      label <- nonSpace
      hsc
      _ <- CharParser.char '|'
      hsc
      attrs <- P.sepBy attr (CharParser.char ',')
      pure (label, Map.fromList attrs)

    attr :: Parser (Text, Text)
    attr = do 
      hsc
      key <- nonSpace
      hsc
      _ <- CharParser.char '='
      hsc
      val <- nonSpace
      hsc
      pure (key, val)
      
-- Charater, punctuation, or unicode thing.
nonSpace :: Parser Text
nonSpace = do 
  val <- P.many $ (P.try CharParser.alphaNumChar) <|> (P.try CharParser.symbolChar) <|> (P.try $ CharParser.char '.')
  pure $ T.pack val


parseBold :: P.Pos -> Parser Content
parseBold pos = do 
  content <- P.between (CharParser.char '*') (CharParser.char '*') $ P.many $ contentParser pos
  _ <- P.optional CharParser.newline
  pure $ Content $ DecoratedText Bold content

parseItalic :: P.Pos -> Parser Content
parseItalic pos = do 
  content <- P.between (CharParser.char '/') (CharParser.char '/') $ P.many $ contentParser pos
  _ <- P.optional CharParser.newline
  pure $ Content $ DecoratedText Italic content

newLineParser :: Parser Content
newLineParser = p
  where
    p = do 
      _ <- P.many spaceChar
      _ <- CharParser.newline
      sc
      pure NewLine

    spaceChar :: Parser Char
    spaceChar = P.satisfy isHSpace <?> "horizontal space"

    isHSpace :: Char -> Bool
    isHSpace x = isSpace x && x /= '\n' && x /= '\r'

textParser :: P.Pos -> Parser Content
textParser pos = p 
  where 
    p = do 
      fstWord <- parseWord
      rest <- P.many (L.indentGuard hsc EQ pos >> parseWord)
      pure $ txt $ T.unwords (fstWord : rest)

    parseWord :: Parser Text
    parseWord = do 
      word <- P.some $ (P.try ourChar) <|> (P.try esc) 
      _ <- P.optional CharParser.newline
      pure $ T.pack $ word

    ourChar :: Parser Char 
    ourChar = P.satisfy validChar 
      where 
        invalidChar :: [Char]
        invalidChar = "|\\[]\r\n/*#"
        validChar x = isPrint x && (not $ List.any (\c -> c == x) invalidChar)

    esc ::Parser Char 
    esc  
      =   (P.try $ CharParser.string "\\*" >>= \_ -> pure '*')
      <|> (P.try $ CharParser.string "\\\\" >>= \_ -> pure '\\')
      <|> (P.try $ CharParser.string "\\|" >>= \_ -> pure '|')
      <|> (P.try $ CharParser.string "\\[" >>= \_ -> pure '[')
      <|> (P.try $ CharParser.string "\\[" >>= \_ -> pure '[')
      <|> (P.try $ CharParser.string "\\/" >>= \_ -> pure '/')
      <|> (P.try $ CharParser.string "\\#" >>= \_ -> pure '#')

    txt :: Text -> Ast.Content
    txt t = Ast.Content $ Ast.TextContent t
