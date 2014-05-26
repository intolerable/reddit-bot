module Examples where

import Classifier
import NaiveBayes

import Reddit.API.Types.Reddit
import qualified Reddit.API as Reddit
import qualified Reddit.API.Types.Post as Post

import Control.Applicative
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Directory as Dir
import qualified System.IO.Strict as Strict

getTrainingData :: Reddit (TrainingData [Text] PostClassification)
getTrainingData = do
  correct <- examplesCorrect
  incorrect <- examplesIncorrect
  return $ TrainingData $ map (\a -> (keywords $ postTextTitle a, Unrelated)) incorrect ++ map (\a -> (keywords $ postTextTitle a, NewPlayer)) correct 

examples :: Reddit [Post.Post]
examples = (++) <$> examplesCorrect <*> examplesIncorrect

exampleIDs :: [Post.PostID]
exampleIDs = exampleIDsCorrect ++ exampleIDsIncorrect

examplesCorrect :: Reddit [Post.Post]
examplesCorrect = mapM getOrLoadPostInfo exampleIDsCorrect

examplesIncorrect :: Reddit [Post.Post]
examplesIncorrect = mapM getOrLoadPostInfo exampleIDsIncorrect

exampleIDsCorrect :: [Post.PostID]
exampleIDsCorrect = map Post.PostID ["24jh8m", "24eme3", "24emhg", "24nsxb", "24opbh", "24o8sh", "24v66k", "24vt4p", "25j3qm", "25le2f", "25jzxx", "25ot8f", "25r8cr", "25sdb5", "25tfu1", "25uxdk", "25vf5q", "25vocm", "25w1dm", "25vi7y", "25wgwf", "25x4ak", "25xcbi", "25xel8", "25xec9", "25xp4n", "266un0", "26iiop"]

exampleIDsIncorrect :: [Post.PostID]
exampleIDsIncorrect = map Post.PostID ["24kokp", "24kpwq", "24kqrz", "24lci7", "24liv1", "24ohpk", "24ot78", "24pban", "24q7t5", "24r432", "24rzmo", "24slxo", "24u3ti", "24vgan", "24vt9t", "256gi7", "256tak", "2570tb", "257752", "2580vf", "2580w8", "25812f", "25816s", "2581ac", "2581ds", "25a9ev", "25aij5", "25audt", "25d8k5", "25h1ed", "25honv", "25hvaj", "25jeyr", "25ku8x", "25m7lk", "25o1ak", "25odgj", "25pc9p", "25q8f9", "25r2yv", "25rbw9", "25shyl", "25sjmt", "25t3ni", "25uql5", "25uveq", "25vb2b", "25xfbm", "25xg07", "25z6qv", "25zbk0", "260nwh", "26c5ck", "26c5f3", "26c5g5", "26c5in", "26c6bf", "26c797", "26c7b6", "26c80x", "26c8hq", "26c8kk", "26c8x4", "26c90u", "26c9fc", "26c9oq", "26c9yv", "26ca21", "26ca6f", "26cadk", "26db1n", "26dcc9", "26dcqw", "26dh1n", "26ih8t", "26ii6j", "26iifz", "26ij34", "26ijhg", "26ijqi", "26ik5k", "26ik7e", "26imn9", "26imol", "26imqv", "26inef", "26inl6", "26inv0", "26inxz", "26iogd", "26iope", "26iowv", "26ioyb", "26ioyj", "26ipve", "26iq8i", "26iqke", "26iqub"]

getOrLoadPostInfo :: Post.PostID -> Reddit Post.Post
getOrLoadPostInfo (Post.PostID p) = do
  let filename = "cached/" ++ Text.unpack p
  exists <- liftIO $ Dir.doesFileExist filename
  if exists
    then do
      f <- liftIO $ Strict.readFile filename
      return $ read f
    else do
      r <- Reddit.getPostInfo (Post.PostID p)
      liftIO $ writeFile filename (show r)
      return r
