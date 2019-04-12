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

moveFour :: String -> String
moveFour = moveTwo . moveTwo -- efficiency?

move :: Int -> String -> String
move 1 xs = moveOne xs
move 2 xs = moveTwo xs
move 3 xs = moveThree xs
move 4 xs = moveFour xs
move _ xs = xs

isVowel :: Char -> Bool
isVowel x = elem x "aeiouAEIOU"

isVowelOrY :: Char -> Bool
isVowelOrY x = elem x "aeiouyAEIOUY"

leadingConsonants :: String -> String
leadingConsonants (x:xs)    | isVowel x = ""
                            | otherwise = [x] ++ (takeWhile (not . isVowelOrY) xs)
leadingConsonants "" = ""

lastConsFirstVowel :: String -> String
lastConsFirstVowel xs   | length (leadingConsonants xs) == 0    = [head xs]
                        | otherwise                             = last (leadingConsonants xs) : head (filter isVowel xs) : []

charsUntilVowel :: String -> Int
charsUntilVowel = length . leadingConsonants

translate :: String -> String
translate xs    | elem ' ' xs                                   = unwords (map translate (words xs))
                | elem (leadingConsonants xs) ["xr", "yttr"]    = theFonz xs
                | (lastConsFirstVowel xs == "qu")               = (theFonz . (move (1 + (charsUntilVowel xs)))) xs
                | otherwise                                     = (theFonz . (move (charsUntilVowel xs))) xs

