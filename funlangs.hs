#!/usr/bin/env stack
-- stack --resolver=lts-12.22 script
{-# OPTIONS
    -Wall -Wcompat -Werror
    -Wincomplete-record-updates -Wincomplete-uni-patterns
    -Wredundant-constraints #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}

import           Data.List (sortOn)
import           Data.Map (Map, assocs, elems, keys, (!?))
import           Data.Ord  (Down (Down))
-- import           Util      (fst3)

languages :: Map String LanguageDesc
languages =
    [ "C" -:
        [ AdHocPolymorphism                     -: No
        , AlgebraicDataTypes                    -: No
        , Closures                              -: No
        , DependentTypes                        -: No
        , DownwardsFunargProblem                -: Yes
        , ForcesImmutability                    -: No
        , ImmutableData                         -: No
        , Laziness                              -: No
        , ParametricModules                     -: No
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: No
        ]
    , "C++" -:
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: No
        , Closures                              -: Yes
        , DependentTypes                        -: No
        , DownwardsFunargProblem                -: Yes
        , ForcesImmutability                    -: No
        , ImmutableData                         -: Yes
        , Laziness                              -: No
        , ParametricModules                     -: No
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Yes
        , PureFunctions                         -: Quirks
        , UpwardsFunargProblem                  -: Quirks
        ]
    , "Haskell" -:
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , Closures                              -: Yes
        , DependentTypes                        -: No
        , DownwardsFunargProblem                -: Yes
        , ForcesImmutability                    -: Yes
        , ImmutableData                         -: Yes
        , Laziness                              -: Yes
        , ParametricModules                     -: Quirks
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Yes
        , PatternMatchingAlternatives           -: Yes
        , PatternMatchingVariableIntroduction   -: Yes
        , PureFunctions                         -: Yes
        , TailCallOptimization                  -: Yes
        , UpwardsFunargProblem                  -: Yes
        ]
    , "Idris" -:
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , Closures                              -: Yes
        , DependentTypes                        -: Yes
        , DownwardsFunargProblem                -: Yes
        , ForcesImmutability                    -: Yes
        , ImmutableData                         -: Yes
        , Laziness                              -: No
        , ParametricModules                     -: Yes
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Yes
        , PatternMatchingAlternatives           -: No
        , PatternMatchingVariableIntroduction   -: Yes
        , PolymorphicRecursion                  -: Yes
        , PureFunctions                         -: Yes
        , ReferentialTransparency               -: Yes
        , TailCallOptimization                  -: No
        , UpwardsFunargProblem                  -: Yes
        ]
    , "OCaml" -:
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , Closures                              -: Yes
        , DependentTypes                        -: No
        , DownwardsFunargProblem                -: Yes
        , ForcesImmutability                    -: Yes
        , ImmutableData                         -: Yes
        , Laziness                              -: Yes
        , ParametricPolymorphism                -: Yes
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: Yes
        ]
    , "Python" -:
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: No
        , Closures                              -: Yes
        , DependentTypes                        -: No
        , DownwardsFunargProblem                -: Yes
        , ForcesImmutability                    -: No
        , ImmutableData                         -: Yes
        , Laziness                              -: No
        , ParametricModules                     -: No
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Yes
        , PatternMatchingVariableIntroduction   -: Yes
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: Yes
        ]
    , "Rust" -:
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , Closures                              -: Quirks
        , DependentTypes                        -: No
        , DownwardsFunargProblem                -: Yes
        , ForcesImmutability                    -: Yes
        , ImmutableData                         -: Yes
        , Laziness                              -: No
        , ParametricModules                     -: No
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Quirks
        , PatternMatchingAlternatives           -: Yes
        , PatternMatchingVariableIntroduction   -: Yes
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: Yes
        ]
    ]

type LanguageDesc = Map Feature Value

data Feature
    = AdHocPolymorphism
    | AlgebraicDataTypes
    | Closures
    | DependentTypes
    | DownwardsFunargProblem
    | ForcesImmutability
    | ImmutableData
    | Laziness
    | ParametricModules
    | ParametricPolymorphism
    | PatternMatching
    | PatternMatchingAlternatives
    | PatternMatchingVariableIntroduction
    | PolymorphicRecursion
    | PureFunctions
    | ReferentialTransparency
    | TailCallOptimization
    | UpwardsFunargProblem
    deriving (Bounded, Enum, Eq, Ord, Show)

data Value = No | Quirks | Yes
instance Show Value where
    show = \case
        No     -> ":x:"
        Quirks -> ":warning:"
        Yes    -> ":heavy_check_mark:"

features :: [Feature]
features = [minBound ..]

main :: IO ()
main = putStrLn . unlines
    $   "<!-- DO NOT EDIT THIS FILE -->"
    :   "<!-- edit funlangs.hs instead -->"
    :   ""
    :   "# Functional languages"
    :   ""
    :   "There is no such thing as a functional language."
    :   "There are only languages with different functional features."
    :   ""
    :   "## Functional features"
    :   ""
    :   [ unwords
            $ "|" : "Feature" : "|"
            : do
                language <- keys languages
                [language, "|"]
        , "|---|" ++ concat (replicate (length languages) "---|")
        ]
    ++  [ unwords
            $ "|" : show feature : "|"
            : do
                languageFeatures <- elems languages
                [maybe "" show $ languageFeatures !? feature, "|"]
        | feature <- features
        ]
    ++  "## Scores"
    :   ""
    :   "| Language | Score |"
    :   "|----------|-------|"
    :   [ unwords ["|", language, "|", show (realToFrac score :: Float), "|"]
        | (score, language) <- sortOn Down
            [ (score, language)
            | (language, languageFeatures) <- assocs languages
            , let score = sum $ value <$> languageFeatures
            ]
        ]

value :: Value -> Rational
value = \case
    No     -> 0
    Quirks -> 1/2
    Yes    -> 1

(-:) :: a -> b -> (a, b)
(-:) = (,)
