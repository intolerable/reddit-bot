module NaiveBayes where

import Data.Ratio (Ratio, (%))
import Data.List
import Data.Ord
--import qualified Data.List as List

data Classifier a = Classifier (a -> Bool)

data TrainingData a b = TrainingData [(a,b)]
  deriving (Show, Read, Eq)

classifyAmong :: (Ord a, Eq b) => [b] -> TrainingData a b -> [Classifier a] -> b
classifyAmong gs t cs = head $ sortBy (comparing (classChance t cs)) gs 

classChance :: Eq b => TrainingData a b -> [Classifier a] -> b -> Rational
classChance t cs g = foldl (*) 1 (map (\c -> probability t c g) cs) * given t g

given :: Eq b => TrainingData a b -> b -> Rational
given (TrainingData t) c = (1 + genericLength count) % genericLength t
  where count = filter ((== c) . snd) t

probability :: (Eq b) => TrainingData a b -> Classifier a -> b -> Rational
probability (TrainingData t) (Classifier f) c = (1 + genericLength fit) % (max (genericLength count) 1)
  where count = filter ((== c) . snd) t 
        fit = filter (f . fst) count

emptyClassifier :: Ord a => TrainingData a b
emptyClassifier = TrainingData []

train :: Ord a => a -> b -> TrainingData a b -> TrainingData a b
train a b (TrainingData m) = TrainingData ((a,b):m)
