module NaiveBayes where

import Data.Ratio ((%))
import Data.List
import Data.Ord

data Classifier a = Classifier (a -> Bool)

data TrainingData a b = TrainingData [(a,b)]
  deriving (Show, Read, Eq)

classify :: (Eq b, Bounded b, Enum b) => TrainingData a b -> [Classifier a] -> b
classify = classifyAmong [minBound .. maxBound]

classifyAmong :: (Eq b) => [b] -> TrainingData a b -> [Classifier a] -> b
classifyAmong gs t cs = head $ reverse $ sortBy (comparing (classChance t cs)) gs 

classChance :: (Eq b) => TrainingData a b -> [Classifier a] -> b -> Rational
classChance t cs g = productPs * pTotal
  where pTotal = given t g
        productPs = foldl' (\r a -> r * probability t a g) 1 cs

given :: (Eq b) => TrainingData a b -> b -> Rational
given (TrainingData t) c = (succ $ genericLength count) % (succ $ genericLength t)
  where count = filter ((== c) . snd) t

probability :: (Eq b) => TrainingData a b -> Classifier a -> b -> Rational
probability (TrainingData t) (Classifier f) c = (succ $ genericLength fit) % (succ $ max (genericLength count) 0)
  where count = filter ((== c) . snd) t 
        fit = filter (f . fst) count

emptyClassifier :: Ord a => TrainingData a b
emptyClassifier = TrainingData []

train :: a -> b -> TrainingData a b -> TrainingData a b
train a b (TrainingData m) = TrainingData ((a,b):m)
