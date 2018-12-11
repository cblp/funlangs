#!/usr/bin/env stack
-- stack --resolver=lts-12.22 script
{-# OPTIONS
    -Wall -Wcompat -Werror
    -Wincomplete-record-updates -Wincomplete-uni-patterns
    -Wredundant-constraints #-}
{-# LANGUAGE OverloadedLists #-}

import           Data.Set (Set, member)

languages :: LanguageDesc
languages =
    [ ("C", [FunctionAsArgument, FunctionAsReturn])
    , ("C++", [Closures, FunctionAsArgument, FunctionAsReturn])
    , ("Haskell", [Closures, FunctionAsArgument, FunctionAsReturn])
    , ("OCaml", [Closures, FunctionAsArgument, FunctionAsReturn])
    , ("Python", [Closures, FunctionAsArgument, FunctionAsReturn])
    , ("Rust", [Closures, FunctionAsArgument, FunctionAsReturn])
    ]

type LanguageDesc = [(String, Set Feature)]

data Feature = Closures | FunctionAsArgument | FunctionAsReturn
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
            $ "|" : language : "|" : show (length languageFeatures) : "|"
            : do
                f <- features
                [if f `member` languageFeatures then yes else no, "|"]
        | (language, languageFeatures) <- languages
        ]
  where
    yes = ":heavy_check_mark:"
    no  = ":x:"
