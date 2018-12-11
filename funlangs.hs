#!/usr/bin/env stack
-- stack --resolver=lts-12.22 script
{-# OPTIONS
    -Wall -Wcompat -Werror
    -Wincomplete-record-updates -Wincomplete-uni-patterns
    -Wredundant-constraints #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE ParallelListComp #-}

import           Data.Char (isUpper)
import           Data.List (sortOn)
import           Data.Ord  (Down (Down))
import           Data.Set  (Set, member)

languages :: LanguageDesc
languages =
    [ ("C", [FunctionAsArgument, FunctionAsReturn])
    , ("C++",
        [ Closures
        , FunctionAsArgument
        , FunctionAsReturn
        , ImmutableData
        , ParametricPolymorphism
        , PatternMatchingDestructuring
        ])
    , ("Haskell",
        [ AdHocPolymorphism
        , AlgebraicDataTypes
        , Closures
        , FunctionAsArgument
        , FunctionAsReturn
        , ImmutableByDefault
        , ImmutableData
        , ParametricPolymorphism
        , PatternMatchingAlternatives
        , PatternMatchingDestructuring
        , PatternMatchingVariableIntroduction
        , TailCallOptimization
        ])
    , ("OCaml",
        [ Closures
        , FunctionAsArgument
        , FunctionAsReturn
        , ImmutableByDefault
        , ImmutableData
        , ParametricPolymorphism
        ])
    , ("Python",
        [ Closures
        , FunctionAsArgument
        , FunctionAsReturn
        , ImmutableData
        , ParametricPolymorphism
        , PatternMatchingDestructuring
        , PatternMatchingVariableIntroduction
        ])
    , ("Rust",
        [ Closures
        , FunctionAsArgument
        , FunctionAsReturn
        , ImmutableByDefault
        , ImmutableData
        , ParametricPolymorphism
        , PatternMatchingAlternatives
        , PatternMatchingDestructuring
        , PatternMatchingVariableIntroduction
        ])
    ]

type LanguageDesc = [(String, Set Feature)]

data Feature
    = AdHocPolymorphism
    | AlgebraicDataTypes
    | Closures
    | DependentTypes
    | FunctionAsArgument
    | FunctionAsReturn
    | ImmutableByDefault
    | ImmutableData
    | ParametricPolymorphism
    | PatternMatchingAlternatives
    | PatternMatchingDestructuring
    | PatternMatchingVariableIntroduction
    | TailCallOptimization
    deriving (Bounded, Enum, Eq, Ord, Show)

features :: [Feature]
features = [minBound ..]

main :: IO ()
main = putStrLn . unlines
    $   "# Functional languages"
    :   ""
    :   "There is no such thing as functional language."
    :   "There are only languages with defferent language features."
    :   ""
    :   [ unwords
            $ "|" : "Language" : "|" : "Overall rating" : "|"
            : concat
                [ [show n ++ '.' : showAbbr feature, "|"]
                | n <- [1 :: Int ..]
                | feature <- features
                ]
        , "|---|---|" ++ concat (replicate (length features) "---|")
        ]
    ++  [ unwords
            $ "|" : language : "|" : show rating : "|"
            : do
                f <- features
                [if f `member` languageFeatures then yes else no, "|"]
        | (rating, language, languageFeatures) <- languages'
        ]
    ++  ""
    :   [ show n ++ ". " ++ showAbbr feature ++ ": " ++ show feature
        | n <- [1 :: Int ..]
        | feature <- features
        ]
  where
    languages' = sortOn Down
        [ (rating, language, languageFeatures)
        | (language, languageFeatures) <- languages
        , let rating = length languageFeatures
        ]
    yes = ":heavy_check_mark:"
    no  = ":x:"

showAbbr :: Show a => a -> String
showAbbr = filter isUpper . show
