module PigLatin (translate) where

translate :: String -> String
translate [] = []
translate (x:xs) | elem x "aeiou"                         = [x] ++ xs ++ "ay"
                 | True                                   = xs ++ [x] ++ "ay"
