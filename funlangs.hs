#!/usr/bin/env stack
-- stack --resolver=lts-12.22 script
{-# OPTIONS
    -Wall -Wcompat -Werror
    -Wincomplete-record-updates -Wincomplete-uni-patterns
    -Wredundant-constraints #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE ParallelListComp #-}

import           Data.Char (isUpper)
import           Data.List (sortOn)
import           Data.Map  (Map, (!?))
import           Data.Ord  (Down (Down))
import           Util      (fst3)

languages :: LanguageDesc
languages =
    [ "C" -: [FunctionAsValue -: Quirks]
    , "C++" -:
        [ Closures -: Yes
        , FunctionAsValue -: Yes
        , ImmutableData -: Yes
        , ParametricPolymorphism -: Yes
        , PatternMatching -: Yes
        ]
    , "Haskell" -:
        [ AdHocPolymorphism -: Yes
        , AlgebraicDataTypes -: Yes
        , Closures -: Yes
        , FunctionAsValue -: Yes
        , ImmutableByDefault -: Yes
        , ImmutableData -: Yes
        , ParametricPolymorphism -: Yes
        , PatternMatching -: Yes
        , PatternMatchingAlternatives -: Yes
        , PatternMatchingVariableIntroduction -: Yes
        , TailCallOptimization -: Yes
        ]
    , "OCaml" -:
        [ AlgebraicDataTypes -: Yes
        , Closures -: Yes
        , FunctionAsValue -: Yes
        , ImmutableByDefault -: Yes
        , ImmutableData -: Yes
        , ParametricPolymorphism -: Yes
        ]
    , "Python" -:
        [ Closures -: Yes
        , FunctionAsValue -: Yes
        , ImmutableData -: Yes
        , ParametricPolymorphism -: Yes
        , PatternMatching -: Yes
        , PatternMatchingVariableIntroduction -: Yes
        ]
    , "Rust" -:
        [ Closures -: Quirks
        , FunctionAsValue -: Yes
        , ImmutableByDefault -: Yes
        , ImmutableData -: Yes
        , ParametricPolymorphism -: Yes
        , PatternMatching -: Quirks
        , PatternMatchingAlternatives -: Yes
        , PatternMatchingVariableIntroduction -: Yes
        ]
    ]

type LanguageDesc = [(String, Map Feature Value)]

data Feature
    = AdHocPolymorphism
    | AlgebraicDataTypes
    | Closures
    | DependentTypes
    | FunctionAsValue
    | ImmutableByDefault
    | ImmutableData
    | Laziness
    | ParametricModules
    | ParametricPolymorphism
    | PatternMatching
    | PatternMatchingAlternatives
    | PatternMatchingVariableIntroduction
    | PolymorphicRecursion
    | ReferentialTransparency
    | TailCallOptimization
    deriving (Bounded, Enum, Eq, Ord, Show)

data Value = No | Quirks | Yes

features :: [Feature]
features = [minBound ..]

main :: IO ()
main = putStrLn . unlines
    $   "# Functional languages"
    :   ""
    :   "There is no such thing as a functional language."
    :   "There are only languages with different functional features."
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
                [   case languageFeatures !? f of
                        Nothing     -> ":grey_question:"
                        Just No     -> ":x:"
                        Just Quirks -> ":white_check_mark:"
                        Just Yes    -> ":heavy_check_mark:"
                    , "|"
                    ]
        | (rating, language, languageFeatures) <- languages'
        ]
    ++  ""
    :   [ show n ++ ". " ++ showAbbr feature ++ ": " ++ show feature
        | n <- [1 :: Int ..]
        | feature <- features
        ]
  where
    languages' = sortOn (Down . fst3)
        [ (rating, language, languageFeatures)
        | (language, languageFeatures) <- languages
        , let rating = sum $ value <$> languageFeatures
        ]

showAbbr :: Show a => a -> String
showAbbr = filter isUpper . show

value :: Value -> Rational
value = \case
    No     -> 0
    Quirks -> 1/2
    Yes    -> 1

(-:) :: a -> b -> (a, b)
(-:) = (,)
