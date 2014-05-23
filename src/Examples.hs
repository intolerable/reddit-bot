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
exampleIDsCorrect = map Post.PostID ["24jh8m", "24eme3", "24emhg", "24nsxb", "24opbh", "24o8sh", "24v66k", "24vt4p", "25j3qm", "25le2f", "25jzxx", "25ot8f", "25r8cr", "25sdb5", "25tfu1", "25uxdk", "25vf5q", "25vocm", "25w1dm", "25vi7y", "25wgwf", "25x4ak", "25xcbi", "25xel8", "25xec9", "25xp4n", "266un0"]

examplesIncorrect :: Reddit [Post.Post]
examplesIncorrect = mapM Reddit.getPostInfo exampleIDsIncorrect

exampleIDsIncorrect :: [Post.PostID]
exampleIDsIncorrect = map Post.PostID ["24kqrz", "24kpwq", "24kokp", "24lci7", "24liv1", "24ot78", "24ohpk", "24pban", "24q7t5", "24r432", "24rzmo", "24slxo", "24u3ti", "24vgan", "24vt9t", "257752", "2570tb", "256tak", "256gi7", "2581ac", "25816s", "25812f", "2580vf", "2580w8", "2581ds", "25aij5", "25a9ev", "25d8k5", "25h1ed", "25honv", "25hvaj", "25jeyr", "25ku8x", "25m7lk", "25o1ak", "25odgj", "25pc9p", "25q8f9", "25sjmt", "25rbw9", "25r2yv", "25shyl", "25t3ni", "25uql5", "25audt", "25uveq", "25vb2b", "25xfbm", "25xg07", "25z6qv", "25zbk0", "260nwh"]
