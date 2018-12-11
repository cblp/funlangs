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
    [ "C" -:
        [ AdHocPolymorphism -: No
        , AlgebraicDataTypes -: No
        , Closures -: No
        , DownwardsFunargProblem -: Yes
        , UpwardsFunargProblem -: No
        ]
    , "C++" -:
        [ AdHocPolymorphism -: Yes
        , AlgebraicDataTypes -: No
        , Closures -: Yes
        , DownwardsFunargProblem -: Yes
        , ImmutableData -: Yes
        , ParametricPolymorphism -: Yes
        , PatternMatching -: Yes
        , UpwardsFunargProblem -: Quirks
        ]
    , "Haskell" -:
        [ AdHocPolymorphism -: Yes
        , AlgebraicDataTypes -: Yes
        , Closures -: Yes
        , DownwardsFunargProblem -: Yes
        , ForcesImmutability -: Yes
        , ImmutableData -: Yes
        , ParametricPolymorphism -: Yes
        , PatternMatching -: Yes
        , PatternMatchingAlternatives -: Yes
        , PatternMatchingVariableIntroduction -: Yes
        , TailCallOptimization -: Yes
        , UpwardsFunargProblem -: Yes
        ]
    , "Idris" -:
        [ DependentTypes -: Yes
        , DownwardsFunargProblem -: Yes
        , UpwardsFunargProblem -: Yes
        ]
    , "OCaml" -:
        [ AdHocPolymorphism -: Yes
        , AlgebraicDataTypes -: Yes
        , Closures -: Yes
        , DownwardsFunargProblem -: Yes
        , ForcesImmutability -: Yes
        , ImmutableData -: Yes
        , ParametricPolymorphism -: Yes
        , UpwardsFunargProblem -: Yes
        ]
    , "Python" -:
        [ AdHocPolymorphism -: Yes
        , AlgebraicDataTypes -: No
        , Closures -: Yes
        , DownwardsFunargProblem -: Yes
        , ImmutableData -: Yes
        , ParametricPolymorphism -: Yes
        , PatternMatching -: Yes
        , PatternMatchingVariableIntroduction -: Yes
        , UpwardsFunargProblem -: Yes
        ]
    , "Rust" -:
        [ AdHocPolymorphism -: Yes
        , AlgebraicDataTypes -: Yes
        , Closures -: Quirks
        , DownwardsFunargProblem -: Yes
        , ForcesImmutability -: Yes
        , ImmutableData -: Yes
        , ParametricPolymorphism -: Yes
        , PatternMatching -: Quirks
        , PatternMatchingAlternatives -: Yes
        , PatternMatchingVariableIntroduction -: Yes
        , UpwardsFunargProblem -: Yes
        ]
    ]

type LanguageDesc = [(String, Map Feature Value)]

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
    | ReferentialTransparency
    | TailCallOptimization
    | UpwardsFunargProblem
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
            $ "|" : language : "|" : show (realToFrac rating :: Float) : "|"
            : do
                f <- features
                [   case languageFeatures !? f of
                        Nothing     -> ""
                        Just No     -> ":x:"
                        Just Quirks -> ":warning:"
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
