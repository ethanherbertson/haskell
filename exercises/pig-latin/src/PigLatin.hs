module PigLatin (translate) where

adday :: String -> String
adday xs = xs ++ "ay"

moveone :: String -> String
moveone (x:xs) = xs ++ [x]
moveone (xs) = xs

movetwo :: String -> String
movetwo = moveone . moveone

movethree :: String -> String
movethree = moveone . movetwo

translate :: String -> String
translate (x:y:z:rest) | elem x "aeiou"           = adday ([x, y, z] ++ rest)
                       | elem [x, y] ["ch", "qu"] = (adday . movetwo) ([x, y, z] ++ rest)
                       | [y, z] == "qu"           = (adday . movethree) ([x, y, z] ++ rest)
                       | True                     = (adday . moveone) ([x, y, z] ++ rest)
translate [] = []
translate (xs) = xs ++ "ay"
