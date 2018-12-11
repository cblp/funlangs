#!/usr/bin/env stack
-- stack --resolver=lts-12.22 script
{-# OPTIONS
    -Wall -Wcompat -Werror
    -Wincomplete-record-updates -Wincomplete-uni-patterns
    -Wredundant-constraints #-}
{-# LANGUAGE OverloadedLists #-}

import           Data.List (sortOn)
import           Data.Ord  (Down (Down))
import           Data.Set  (Set, member)

languages :: LanguageDesc
languages =
    [ ("C", [FunctionAsArgument, FunctionAsReturn])
    , ("C++", [Closures, FunctionAsArgument, FunctionAsReturn])
    , ("Haskell",
        [Closures, FunctionAsArgument, FunctionAsReturn, ListComprehension])
    , ("OCaml", [Closures, FunctionAsArgument, FunctionAsReturn])
    , ("Python",
        [Closures, FunctionAsArgument, FunctionAsReturn, ListComprehension])
    , ("Rust", [Closures, FunctionAsArgument, FunctionAsReturn])
    ]

type LanguageDesc = [(String, Set Feature)]

data Feature
    = Closures
    | FunctionAsArgument
    | FunctionAsReturn
    | ListComprehension
    deriving (Bounded, Enum, Eq, Ord, Show)

features :: [Feature]
features = [minBound ..]

main :: IO ()
main = putStrLn . unlines
    $   [ unwords
            $ "|" : "Language" : "|" : "Overall rating" : "|"
            : concat [[show feature, "|"] | feature <- features]
        , "|---|---|" ++ concat (replicate (length features) "---|")
        ]
    ++  [ unwords
            $ "|" : language : "|" : show rating : "|"
            : do
                f <- features
                [if f `member` languageFeatures then yes else no, "|"]
        | (rating, language, languageFeatures) <- languages'
        ]
  where
    languages' = sortOn Down
        [ (rating, language, languageFeatures)
        | (language, languageFeatures) <- languages
        , let rating = length languageFeatures
        ]
    yes = ":heavy_check_mark:"
    no  = ":x:"
