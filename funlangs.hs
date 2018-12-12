{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}

import           Data.List (intercalate, sortOn)
import           Data.Map  (Map, assocs, elems, keys, (!?))
import           Data.Ord  (Down (Down))

languages :: Map String Desc
languages =
    [ "C" -: Desc
        [ Closures                              -: No
        , DownwardsFunargProblem                -: Yes
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: No
        ]
        [ AdHocPolymorphism                     -: No
        , AlgebraicDataTypes                    -: No
        , DependentTypes                        -: No
        , ForcesImmutability                    -: No
        , ImmutableData                         -: No
        , Laziness                              -: No
        , ParametricModules                     -: No
        ]
    , "C++" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , PureFunctions                         -: Quirks
        , UpwardsFunargProblem                  -: Quirks
        ]
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: No
        , DependentTypes                        -: No
        , ForcesImmutability                    -: No
        , ImmutableData                         -: Yes
        , Laziness                              -: No
        , ParametricModules                     -: No
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Yes
        ]
    , "Haskell" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , PureFunctions                         -: Yes
        , TailCallOptimization                  -: Yes
        , UpwardsFunargProblem                  -: Yes
        ]
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , DependentTypes                        -: No
        , ForcesImmutability                    -: Yes
        , ImmutableData                         -: Yes
        , Laziness                              -: Yes
        , ParametricModules                     -: Quirks
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Yes
        , PatternMatchingAlternatives           -: Yes
        , PatternMatchingVariableIntroduction   -: Yes
        ]
    , "Idris" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , PolymorphicRecursion                  -: Yes
        , PureFunctions                         -: Yes
        , TailCallOptimization                  -: No
        , UpwardsFunargProblem                  -: Yes
        ]
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , DependentTypes                        -: Yes
        , ForcesImmutability                    -: Yes
        , ImmutableData                         -: Yes
        , Laziness                              -: No
        , ParametricModules                     -: Yes
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Yes
        , PatternMatchingAlternatives           -: No
        , PatternMatchingVariableIntroduction   -: Yes
        , ReferentialTransparency               -: Yes
        ]
    , "OCaml" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: Yes
        ]
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , DependentTypes                        -: No
        , ForcesImmutability                    -: Yes
        , ImmutableData                         -: Yes
        , Laziness                              -: Yes
        , ParametricPolymorphism                -: Yes
        ]
    , "Python" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: Yes
        ]
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: No
        , DependentTypes                        -: No
        , ForcesImmutability                    -: No
        , ImmutableData                         -: Yes
        , Laziness                              -: No
        , ParametricModules                     -: No
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Yes
        , PatternMatchingVariableIntroduction   -: Yes
        ]
    , "Rust" -: Desc
        [ Closures                              -: Quirks
        , DownwardsFunargProblem                -: Yes
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: Yes
        ]
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , DependentTypes                        -: No
        , ForcesImmutability                    -: Yes
        , ImmutableData                         -: Yes
        , Laziness                              -: No
        , ParametricModules                     -: No
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Quirks
        , PatternMatchingAlternatives           -: Yes
        , PatternMatchingVariableIntroduction   -: Yes
        ]
    ]

data Desc = Desc
    { functional    :: Map FunctionalFeature    Value
    , nonFunctional :: Map NonFunctionalFeature Value
    }

data FunctionalFeature
    = Closures
    | DownwardsFunargProblem
    | PolymorphicRecursion
    | PureFunctions
    | TailCallOptimization
    | UpwardsFunargProblem
    deriving (Bounded, Enum, Eq, Ord, Show)

data NonFunctionalFeature
    = AdHocPolymorphism
    | AlgebraicDataTypes
    | DependentTypes
    | ForcesImmutability
    | ImmutableData
    | Laziness
    | ParametricModules
    | ParametricPolymorphism
    | PatternMatching
    | PatternMatchingAlternatives
    | PatternMatchingVariableIntroduction
    | ReferentialTransparency
    deriving (Bounded, Enum, Eq, Ord, Show)

data Value = No | Quirks | Yes
instance Show Value where
    show = \case
        No     -> ":x:"
        Quirks -> ":warning:"
        Yes    -> ":heavy_check_mark:"

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
    :   row ("Feature" : keys languages)
    :   ("|---|" ++ concat (replicate (length languages) "---|"))
    :   [ row
            $   show feature
            :   [ maybe "" show $ functional languageFeatures !? feature
                | languageFeatures <- elems languages
                ]
        | feature <- universe
        ]
    ++  ""
    :   "## Non-functional features"
    :   ""
    :   row ("Feature" : keys languages)
    :   ("|---|" ++ concat (replicate (length languages) "---|"))
    :   [ row
            $   show feature
            :   [ maybe "" show $ nonFunctional languageFeatures !? feature
                | languageFeatures <- elems languages
                ]
        | feature <- universe
        ]
    ++  ""
    :   "## Scores"
    :   ""
    :   "| Language | Score |"
    :   "|----------|-------|"
    :   [ row [language, show (realToFrac score :: Float)]
        | (score, language) <- sortOn Down
            [ (score, language)
            | (language, Desc{functional}) <- assocs languages
            , let score = sum $ value <$> functional
            ]
        ]

value :: Value -> Rational
value = \case
    No     -> 0
    Quirks -> 1/2
    Yes    -> 1

(-:) :: a -> b -> (a, b)
(-:) = (,)

row :: [String] -> String
row = ("| " ++) . (++ " |") . intercalate " | "

universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]
