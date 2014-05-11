module Main
  ( module Main
  , module Export ) where

import Classifier as Export
import Examples as Export
import Parser as Export
import qualified Markdown as Md

import Control.Monad (unless, when, void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Monoid
import Data.Text (Text)
import Reddit.API
import Reddit.API.Types
import Reddit.API.Types.Listing
import Reddit.API.Types.Subreddit
import Reddit.API.Types.User (Username(..))
import Reddit.Bot
import System.Environment (getArgs)
import System.Exit (exitFailure)
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

dealWith :: Post -> Reddit ()
dealWith p = do
  wl <- liftIO $ getWordList
  when (shouldBotRespond wl p) $ replyWithHelp p

replyWithHelp :: Post -> Reddit ()
replyWithHelp post = do
  cs <- getComments $ Post.postID post
  unless (any ((Username "intolerable-bot" ==) . Comment.author) cs) $  do
    liftIO $ putStrLn $ mconcat ["Replying to ", show $ Post.postID post]
    bodyFile <- liftIO $ readFile "reply.md"
    void $ rateLimit $ reply post $ T.pack bodyFile
