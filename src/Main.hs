module Main where

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

head' :: [a] -> a
head' = foldr1 (\x _ -> x)