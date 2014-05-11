module Markdown where

import Control.Monad.Writer.Strict
import Data.Text (Text)

type Markdown = Writer Text ()

block :: Markdown -> Markdown
block t = do
  t
  tell "\n\n"

bold :: Markdown -> Markdown
bold = surround (tell "**")

italic :: Markdown -> Markdown
italic = surround (tell "*")

surround :: Markdown -> Markdown -> Markdown
surround s m = s >> m >> s

p :: Text -> Markdown
p = tell

linkTo :: Text -> Text -> Markdown
linkTo text link = do
  tell "["
  tell text
  tell "]("
  tell link
  tell ")"

markdownDoc :: Markdown
markdownDoc = do
  block $ do
    "hello" `linkTo` "http://google.com"
  block $ p "hello world"

makeMarkdown :: Markdown -> Text
makeMarkdown = execWriter
