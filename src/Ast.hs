module Ast
    ( Expression(..)
    , Block (..)
    , Content (..)
    , TextContent(..)
    , Decoration(..)
    , List(..)
    , Ast
    ) where

import RIO

type Ast = [Expression]

type Label = Text
type Attributes = [Text]

data TextContent
  = TextContent Text
  | DecoratedText Decoration [Content]
  deriving (Show, Eq)

data Decoration
  = Bold
  | Italic
  | Highlight
  deriving (Show, Eq)

data Content 
  = Directive { directiveLabel :: Text, directiveAttributes :: Map Text Text, directiveContent :: Content }
  | NewLine
  | Content TextContent 
  | FormattedContent { formatContentLabel :: Text, formattedContentAttributes :: Map Text [Text], formattedContentContent :: Text }
  deriving (Show, Eq)

data Expression
  = ExprContent Content
  | ExprBlock Block
  | ExprList List
  deriving (Show, Eq)

data Block
  = Block Label Attributes [Expression] 
  deriving (Show, Eq)

data List 
  = Item Content
  | List
  deriving (Show, Eq)


