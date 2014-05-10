module Examples where

import qualified Reddit.API as Reddit
import Reddit.API.Types.Reddit
import qualified Reddit.API.Types.Post as Post

import Control.Applicative

examples :: Reddit [Post.Post]
examples = (++) <$> examplesCorrect <*> examplesIncorrect

exampleIDs :: [Post.PostID]
exampleIDs = exampleIDsCorrect ++ exampleIDsIncorrect

examplesCorrect :: Reddit [Post.Post]
examplesCorrect = mapM Reddit.getPostInfo exampleIDsCorrect

exampleIDsCorrect :: [Post.PostID]
exampleIDsCorrect = map Post.PostID ["24jh8m", "24eme3", "24emhg", "24nsxb", "24opbh", "24o8sh", "24rn4z", "24v66k", "24vt4p"]

examplesIncorrect :: Reddit [Post.Post]
examplesIncorrect = mapM Reddit.getPostInfo exampleIDsIncorrect

exampleIDsIncorrect :: [Post.PostID]
exampleIDsIncorrect = map Post.PostID ["24kqrz", "24kpwq", "24kokp", "24lci7", "24liv1", "24ot78", "24ohpk", "24pban", "24q7t5", "24r432", "24rzmo", "24seij", "24slxo", "24u3ti", "24vgan", "24vt9t", "257752", "2570tb", "256tak", "256gi7"]
