module Util where

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

fromRight :: Show a => Either a b -> b
fromRight (Right x) = x
fromRight (Left x) = error $ "called fromRight on a Left: " ++ show x

eitherToList :: Either [a] b -> [a]
eitherToList (Right _) = []
eitherToList (Left xs) = xs