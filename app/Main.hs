module Main where

import Data.Char (isAlpha, toLower)
import qualified Data.Set as S


type Symbol   = Char
type WordText = String

data Punctuation
  = Dot    
  | Comma  
  | Excl   
  | Quest  
  deriving (Show, Eq)

newtype Sentence = Sentence [WordText]
  deriving (Show, Eq)

isSentencePunct :: Symbol -> Bool
isSentencePunct c = c `elem` (".!?" :: String)

trim :: String -> String
trim = dropWhileEnd . dropWhileStart
  where
    dropWhileStart = dropWhile (`elem` (" \t\n\r" :: String))
    dropWhileEnd   = reverse . dropWhile (`elem` (" \t\n\r" :: String)) . reverse

splitSentencesRaw :: String -> [String]
splitSentencesRaw = go "" []
  where
    go acc res [] =
      let acc' = trim acc
      in if null acc' then res else res ++ [acc']

    go acc res (c:cs)
      | isSentencePunct c =
          let acc' = trim acc
          in if null acc'
             then go "" res cs
             else go "" (res ++ [acc']) cs
      | otherwise =
          go (acc ++ [c]) res cs

sentenceToWords :: String -> [WordText]
sentenceToWords s =
  let lower = map toLower s
      groups = wordsBy (not . isAlpha) lower
  in filter (not . null) groups

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p str = case dropWhile p str of
  "" -> []
  s' ->
    let (w, rest) = break p s'
    in w : wordsBy p rest

parseSentences :: String -> [Sentence]
parseSentences txt =
  [ Sentence (sentenceToWords raw) | raw <- splitSentencesRaw txt ]


findUniqueWordInFirst :: [Sentence] -> [WordText]
findUniqueWordInFirst [] = []
findUniqueWordInFirst (Sentence firstWords : rest) =
  let firstSet   = S.fromList firstWords
      otherWords = concat [ws | Sentence ws <- rest]
      otherSet   = S.fromList otherWords
  in S.toList (firstSet `S.difference` otherSet)


main :: IO ()
main = do
  putStrLn "Reading file input.txt ..."
  contents <- readFile "input.txt"

  let sentences = parseSentences contents
  putStrLn $ "Number of sentences found: " ++ show (length sentences)

  case sentences of
    [] -> putStrLn "No sentences found in file."

    (Sentence w1 : _) -> do
      putStrLn "Words in the first sentence:"
      print w1

      let uniq = findUniqueWordInFirst sentences
      putStrLn "Words that appear in the first sentence, but not in any other:"
      case uniq of
        [] -> putStrLn "No unique words."
        xs -> print xs
