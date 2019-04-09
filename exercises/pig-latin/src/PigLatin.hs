module PigLatin (translate) where

theFonz :: String -> String
theFonz [] = []
theFonz s = s ++ "ay"

moveOne :: String -> String
moveOne (x:xs) = xs ++ [x]
moveOne s = s

moveTwo :: String -> String
moveTwo = moveOne . moveOne -- efficiency?

moveThree :: String -> String
moveThree = moveOne . moveTwo -- efficiency?

-- TODO: Should probably handle upper/lower case...
translate :: String -> String
translate s@(x:y:z:_) | elem ' ' s               = unwords (map translate (words s))
                      | elem x "aeiou"           = theFonz s
                      | elem [x, y] ["yt", "xr"] = theFonz s
                      | elem [x, y] ["qu"]       = (theFonz . moveTwo) s
                      | [y, z] == "qu"           = (theFonz . moveThree) s
                      | True                     = (theFonz . moveOne) s
translate xs = theFonz xs
