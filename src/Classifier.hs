module Classifier where

import Parser
import NaiveBayes 

import Data.Char (isAlpha, isSpace)
import Data.List
import Data.Monoid
import Data.Text (Text)
import Reddit.API.Types.Post (Post, PostContent(..))
import Text.Parsec (parse)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Reddit.API.Types.Post as Post

data PostClassification = Unrelated
                        | NewPlayer
  deriving (Show, Read, Eq, Enum, Bounded)

getWordList :: IO WordList
getWordList = do
  let filename = "words.list"
  f <- readFile filename
  case parse wordList filename $ Text.pack f of 
    Left _ -> error "Invalid format for words.list"
    Right x -> return $ x

generateClassifiers :: Post -> [Classifier [Text]]
generateClassifiers p =
  map (\w -> Classifier $ \ks -> [w] `isInfixOf` ks) $ nub $ keywords $ postTextTitle p

keywords :: Text -> [Text]
keywords = filter (not . Text.null) . map (Text.filter (isAlpha)) . Text.split (isSpace) . Text.toLower

shouldBotRespond :: TrainingData [Text] PostClassification -> Post -> Bool
shouldBotRespond t post = hasEnoughWords post && isSelfPost post && classify t (generateClassifiers post) == NewPlayer

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
