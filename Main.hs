module Main
  ( module Main
  , module Export ) where

import Classifier as Export
import Examples as Export
import NaiveBayes as Export
import Parser as Export

import Control.Monad (forM_, unless, when, void)
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
import qualified Data.Text as Text
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
noobBot = Bot $ do
  liftIO $ putStrLn "Pulling training data..."
  trainingData <- getTrainingData
  liftIO $ putStrLn "Done."
  every 600 $ rateLimit $ do
    liftIO $ putStrLn "Checking..."
    Listing posts <- Reddit.getNewSubredditPosts (R "DotA2")
    mapM_ (dealWith trainingData) posts
    liftIO $ putStrLn "Done, sleeping for ten minutes."

dealWith :: TrainingData [Text] PostClassification -> Post -> Reddit ()
dealWith t p = do
  when (shouldBotRespond t p) $ replyWithHelp p

replyWithHelp :: Post -> Reddit ()
replyWithHelp post = do
  cs <- getComments $ Post.postID post
  unless (any ((Username "intolerable-bot" ==) . Comment.author) cs) $ do
    liftIO $ putStrLn $ mconcat ["Replying to ", show $ Post.postID post]
    bodyFile <- liftIO $ readFile "reply.md"
    void $ rateLimit $ reply post $ T.pack bodyFile

checkForSummons :: PostID -> Reddit ()
checkForSummons p = do
  comments <- getComments p
  forM_ comments $ \c -> do 
    when (isCommentSummoning c) $ do
      return ()

haveAlreadyReplied :: Thing a => a -> Reddit Bool
haveAlreadyReplied = do
  undefined

isCommentSummoning :: Comment -> Bool
isCommentSummoning c = 
  any (`Text.isInfixOf` Text.toLower (Comment.body c)) incantations
  where
    incantations = map Text.toLower [ "/u/intolerable-bot, I choose you!"
                                    , "summon /u/intolerable-bot"
                                    , "storm, earth and /u/intolerable-bot, heed my call!" ]
