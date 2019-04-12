module PigLatin (translate) where

-- Move n elements from front of array to end of it:
rotate :: Int -> [a] -> [a]
rotate n xs =  take (length xs) (drop n (cycle xs))

-- TODO: Nothing below here can handle uppercase...
isVowel :: Char -> Bool
isVowel x = elem x "aeiou"

isVowelOrY :: Char -> Bool
isVowelOrY x = elem x "aeiouy"

-- Return the leading consonants, if any. Note that both 'x' and 'y' can
-- operate as vowels at the starts of words, e.g. in "xray" and "yttria".
-- Specifically, if and only if they're followed by more consonants.
-- Also note that this function treats the 'u' in 'qu' as a vowel.
--
-- TODO: This should really treat the 'u' in 'qu' as a consonant...
--
leadingCons :: String -> String
leadingCons "" = ""
leadingCons (x:xs)  | isVowel x                                     = ""
                    | elem x "xy" && ((not . isVowelOrY . head) xs) = "" -- e.g. "xray" or "yttria", but not "xylocarp"
                    | otherwise                                     = [x] ++ (takeWhile (not . isVowelOrY) xs)

-- Rotate characters in the word until you get to the first vowel sound:
rotateToFirstVowel :: String -> String
rotateToFirstVowel xs = rotate (length (leadingCons xs)) xs

-- Get the last of the leading consonants, and the vowel that follows it:
lastConFirstVowel :: String -> String
lastConFirstVowel xs    | cons == ""    = ""
                        | cons == xs    = ""
                        | otherwise     = take 2 (drop (length cons - 1) xs)
    where cons = leadingCons xs

-- Translate to Pig Latin. By convention, keep any "qu"s together:
translate :: String -> String
translate xs    | elem ' ' xs                   = unwords (map translate (words xs))
                | lastConFirstVowel xs == "qu"  = ((rotate 1) . rotateToFirstVowel) xs ++ "ay"
                | otherwise                     = rotateToFirstVowel xs ++ "ay"

