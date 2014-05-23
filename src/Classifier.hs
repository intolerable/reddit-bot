module Classifier where

import Parser

import Data.Char (isAlpha, toLower)
import Data.List (genericLength, isPrefixOf)
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import Reddit.API.Types.Post (Post, PostContent(..))
import Text.Parsec (parse)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Reddit.API.Types.Post as Post

data PostClassification = Unrelated
                        | NewPlayer
                        | LoLSwitch
  deriving (Show, Read, Eq)

getWordList :: IO WordList
getWordList = do
  let filename = "words.list"
  f <- readFile filename
  case parse wordList filename $ Text.pack f of 
    Left _ -> error "Invalid format for words.list"
    Right x -> return $ x

keywords :: Text -> [Text]
keywords = filter (not . Text.null) . map (Text.filter (isAlpha)) . Text.split (isSpace) . Text.toLower

shouldBotRespond :: WordList -> Post -> Bool
shouldBotRespond w post = score w post > 0.3 && hasEnoughWords post && isSelfPost post

hasEnoughWords :: Post -> Bool
hasEnoughWords p = length (keywords $ postTextTitle p) > 20

score :: WordList -> Post -> Double
score w post = helper $ postTextTitle post
  where helper t = (total w (keywords t)) / genericLength (keywords t)

postTextTitle :: Post -> Text
postTextTitle p =
  Post.title p <> " " <> case Post.content p of
    SelfPost s _ -> s
    Link _ -> ""
    TitleOnly -> ""

isSelfPost :: Post -> Bool
isSelfPost post = 
  case Post.content post of 
    SelfPost _ _ -> True
    Link _ -> False
    TitleOnly -> False

total :: WordList -> [Text] -> Double
total w ts = fromIntegral . sum $ map (\(ws, value) -> (ws `countInstancesIn` ts) * value) (Map.toList w)  

countInstancesIn :: Eq a => [a] -> [a] -> Integer
countInstancesIn [] _ = 0
countInstancesIn _ [] = 0
countInstancesIn x y = (f $ x `isPrefixOf` y) + countInstancesIn x (tail y)
  where f False = 0
        f True = 1
