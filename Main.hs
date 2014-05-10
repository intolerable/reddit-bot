module Main
  ( module Main
  , module Export ) where

import Examples as Export
import Parser as Export

import Control.Monad (unless, when,)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Data.Char (isAlpha, toLower)
import Data.List (genericLength, isPrefixOf)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Reddit.API
import Reddit.API.Types
import Reddit.API.Types.Listing
import Reddit.API.Types.Post (PostContent(..))
import Reddit.API.Types.Subreddit
import Reddit.API.Types.User (Username(..))
import Reddit.Bot
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Parsec
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Reddit.API as Reddit
import qualified Reddit.API.Types.Post as Post
import qualified Reddit.API.Types.Comment as Comment

type WordListM = ReaderT Text Reddit

main :: IO ()
main = do
  args <- getArgs
  case args of
    pass:[] -> runBots bots "intolerable-bot" (T.pack pass)
    _ -> do
      putStrLn "Usage: reddit-bot PASSWORD"
      exitFailure

bots :: [Bot]
bots = return noobBot

noobBot :: Bot
noobBot = Bot $ every 600 $ rateLimit $ do
  liftIO $ putStrLn "Checking..."
  Listing posts <- Reddit.getNewSubredditPosts (R "DotA2")
  mapM_ dealWith posts

readWordList :: Reddit WordList
readWordList = do
  f <- liftIO $ readFile "words.list"
  case parse wordList "words.list" (T.pack f) of
    Left _ -> Reddit . EitherT . return $ Left $ ParseError "err"
    Right x -> Reddit . EitherT . return $ Right x

dealWith :: Post -> Reddit ()
dealWith p = do
  wl <- readWordList
  when (shouldBotRespond wl p) $ replyWithHelp p

shouldBotRespond :: WordList -> Post -> Bool
shouldBotRespond w post = score w post > 1.25 && hasEnoughWords post

hasEnoughWords :: Post -> Bool
hasEnoughWords p = length (keywords $ postTextTitle p)> 15

score :: WordList -> Post -> Double
score w post = helper $ postTextTitle post
  where helper t = (total w (keywords t)) / genericLength (keywords t)

postTextTitle :: Post -> Text
postTextTitle p =
  Post.title p <> " " <> case Post.content p of
    SelfPost s _ -> s
    Link _ -> ""
    TitleOnly -> ""

keywords :: Text -> [Text]
keywords t = filter (not . T.null) . T.split (not . isAlpha) . T.map toLower $ t 

total :: WordList -> [Text] -> Double
total w ts = fromIntegral . sum $ map (\(ws, value) -> (ws `countInstancesIn` ts) * value) (M.toList w)  


keywordLookup :: WordList -> Text -> Double
keywordLookup w k = fromIntegral $ fromMaybe 1 $ M.lookup [k] w

replyWithHelp :: Post -> Reddit ()
replyWithHelp post = do
  cs <- getComments $ Post.postID post
  unless (any ((Username "intolerable-bot" ==) . Comment.author) cs) $  do
    liftIO $ putStrLn $ mconcat ["Replying to ", show $ Post.postID post]
    --bodyFile <- liftIO $ readFile "reply.md"
    --void $ rateLimit $ reply post $ T.pack bodyFile

countInstancesIn :: Eq a => [a] -> [a] -> Integer
countInstancesIn [] _ = 0
countInstancesIn _ [] = 0
countInstancesIn x y = (f $ x `isPrefixOf` y) + countInstancesIn x (tail y)
  where f False = 0
        f True = 1

--score' :: Text -> 
