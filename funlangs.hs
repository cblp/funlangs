{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}

import           Data.Char (isUpper)
import           Data.List (intercalate, sortOn)
import           Data.Map  (Map, assocs, elems, keys, (!?))
import           Data.Ord  (Down (Down))

languages :: Map String Desc
languages =
    [ "Assembler" -: Desc
        [ Closures                              -: No
        , DownwardsFunargProblem                -: No
        , Functions                             -: No
        , LambdaAbstractionSyntax               -: No
        , PolymorphicRecursion                  -: No
        , PureFunctions                         -: No
        , TailCallOptimization                  -: No
        , UpwardsFunargProblem                  -: No
        ]
        []
    , "C" -: Desc
        [ Closures                              -: No
        , DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , LambdaAbstractionSyntax               -: No
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
        , TotalityChecking                      -: No
        , UniquenessTypes                       -: No
        , UniversePolymorphism                  -: No
        ]
    , "C++" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , LambdaAbstractionSyntax               -: Yes
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
        , PatternMatching                       -: Quirks
        , TotalityChecking                      -: No
        , UniquenessTypes                       -: No
        , UniversePolymorphism                  -: No
        ]
    , "Haskell" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , LambdaAbstractionSyntax               -: Yes
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
        , TotalityChecking                      -: No
        , UniquenessTypes                       -: No
        , UniversePolymorphism                  -: No
        ]
    , "Idris" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , LambdaAbstractionSyntax               -: Yes
        , PolymorphicRecursion                  -: Yes
        , PureFunctions                         -: Yes
        , TailCallOptimization                  -: Yes
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
        , TotalityChecking                      -: Yes
        , UniquenessTypes                       -: Yes
        , UniversePolymorphism                  -: Yes
        ]
    , "Java" -: Desc
        [ DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , LambdaAbstractionSyntax               -: Yes
        , PureFunctions                         -: No
        , TailCallOptimization                  -: No
        , UpwardsFunargProblem                  -: Yes
        ]
        []
    , "OCaml" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , LambdaAbstractionSyntax               -: Yes
        , PolymorphicRecursion                  -: Yes
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: Yes
        ]
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , DependentTypes                        -: No
        , ForcesImmutability                    -: Yes
        , ImmutableData                         -: Yes
        , Laziness                              -: No
        , ParametricPolymorphism                -: Yes
        , TotalityChecking                      -: No
        , UniquenessTypes                       -: No
        , UniversePolymorphism                  -: No
        ]
    , "Python" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , LambdaAbstractionSyntax               -: Quirks
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
        , PatternMatching                       -: Quirks
        , PatternMatchingVariableIntroduction   -: Yes
        , TotalityChecking                      -: No
        , UniquenessTypes                       -: No
        , UniversePolymorphism                  -: No
        ]
    , "Rust" -: Desc
        [ Closures                              -: Quirks
        , DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , LambdaAbstractionSyntax               -: Yes
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
        , TotalityChecking                      -: No
        , UniquenessTypes                       -: Yes
        , UniversePolymorphism                  -: No
        ]
    , "Scala" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , LambdaAbstractionSyntax               -: Yes
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: Yes
        ]
        []
    ]

data Desc = Desc
    { functional    :: Map FunctionalFeature    Value
    , nonFunctional :: Map NonFunctionalFeature Value
    }

data FunctionalFeature
    = Closures
    | DownwardsFunargProblem
    | Functions
    | LambdaAbstractionSyntax
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
    | TotalityChecking
    | UniquenessTypes
    | UniversePolymorphism
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
            $   wordsSpace (show feature)
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
            $   wordsSpace (show feature)
            :   [ maybe "" show $ nonFunctional languageFeatures !? feature
                | languageFeatures <- elems languages
                ]
        | feature <- universe
        ]
    ++  ""
    :   "## Scores"
    :   ""
    :   "A well implemented feature counts as 1,"
    :   "a badly implemented counts as 0.5."
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

wordsSpace :: String -> String
wordsSpace = unwords . splitCamel

splitCamel :: String -> [String]
splitCamel = snd . foldr go (False, []) where
    go c (nextIsUpper, rest) =
        (isUpper c, if nextIsUpper then [c] : rest else pushHead c rest)
    pushHead c = \case
        []   -> [[c]]
        x:xs -> (c:x):xs
