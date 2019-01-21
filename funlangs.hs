{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}

import           Data.Char (isUpper)
import           Data.List (intercalate, sortOn)
import           Data.Map  (Map, assocs, elems, keys, (!?))
import           Data.Ord  (Down (Down))

languages :: Map String Desc
languages =
    [ "C" -: Desc
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
        , LazinessByDefault                     -: No
        , ParametricModules                     -: No
        , PolymorphicRecursion                  -: No
        , RowPolymorphism                       -: No
        , StaticTyping                          -: Quirks
        , TotalityChecking                      -: No
        , UniquenessTypes                       -: No
        , UniversePolymorphism                  -: No
        ]
    , "C++" -: Desc
        [ Closures                              -: Quirks
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
        , LazinessByDefault                     -: No
        , ParametricModules                     -: No
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Quirks
        , PolymorphicRecursion                  -: No
        , RowPolymorphism                       -: No
        , StaticTyping                          -: Yes
        , TotalityChecking                      -: No
        , UniquenessTypes                       -: No
        , UniversePolymorphism                  -: No
        ]
    , "Haskell" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , GuaranteedTailCallOptimization        -: Yes
        , LambdaAbstractionSyntax               -: Yes
        , PureFunctions                         -: Yes
        , UpwardsFunargProblem                  -: Yes
        ]
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , DependentTypes                        -: No
        , ForcesImmutability                    -: Yes
        , ImmutableData                         -: Yes
        , LazinessByDefault                     -: Yes
        , ParametricModules                     -: Quirks
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Yes
        , PatternMatchingAlternatives           -: Yes
        , PatternMatchingVariableIntroduction   -: Yes
        , PolymorphicRecursion                  -: Yes
        , ReferentialTransparency               -: Yes
        , RowPolymorphism                       -: Quirks
        , StaticTyping                          -: Yes
        , TotalityChecking                      -: No
        , UniquenessTypes                       -: No
        , UniversePolymorphism                  -: No
        ]
    , "Idris" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , GuaranteedTailCallOptimization        -: Yes
        , LambdaAbstractionSyntax               -: Yes
        , PureFunctions                         -: Yes
        , UpwardsFunargProblem                  -: Yes
        ]
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , DependentTypes                        -: Yes
        , ForcesImmutability                    -: Yes
        , ImmutableData                         -: Yes
        , LazinessByDefault                     -: No
        , ParametricModules                     -: Yes
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Yes
        , PatternMatchingAlternatives           -: Yes
        , PatternMatchingVariableIntroduction   -: Yes
        , PolymorphicRecursion                  -: Yes
        , ReferentialTransparency               -: Yes
        , StaticTyping                          -: Yes
        , TotalityChecking                      -: Yes
        , UniquenessTypes                       -: Yes
        , UniversePolymorphism                  -: Yes
        ]
    , "Java" -: Desc
        [ DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , GuaranteedTailCallOptimization        -: No
        , LambdaAbstractionSyntax               -: Yes
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: Yes
        ]
        [ ParametricPolymorphism                -: Yes
        , PolymorphicRecursion                  -: No
        , RowPolymorphism                       -: No
        , StaticTyping                          -: Yes
        ]
    , "OCaml" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , GuaranteedTailCallOptimization        -: Yes
        , LambdaAbstractionSyntax               -: Yes
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: Yes
        ]
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , DependentTypes                        -: No
        , ForcesImmutability                    -: Yes
        , ImmutableData                         -: Yes
        , LazinessByDefault                     -: No
        , ParametricModules                     -: Yes
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Yes
        , PatternMatchingAlternatives           -: Yes
        , PolymorphicRecursion                  -: Yes
        , RowPolymorphism                       -: Yes
        , StaticTyping                          -: Yes
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
        , LazinessByDefault                     -: No
        , ParametricModules                     -: No
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Quirks
        , PatternMatchingVariableIntroduction   -: Yes
        , PolymorphicRecursion                  -: Quirks
        , RowPolymorphism                       -: No
        , StaticTyping                          -: No
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
        , LazinessByDefault                     -: No
        , ParametricModules                     -: No
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Quirks
        , PatternMatchingAlternatives           -: Yes
        , PatternMatchingVariableIntroduction   -: Yes
        , StaticTyping                          -: Yes
        , TotalityChecking                      -: No
        , UniquenessTypes                       -: Yes
        , UniversePolymorphism                  -: No
        ]
    , "Scala" -: Desc
        [ Closures                              -: Yes
        , DownwardsFunargProblem                -: Yes
        , Functions                             -: Yes
        , GuaranteedTailCallOptimization        -: Quirks
        , LambdaAbstractionSyntax               -: Yes
        , PureFunctions                         -: No
        , UpwardsFunargProblem                  -: Yes
        ]
        [ AdHocPolymorphism                     -: Yes
        , AlgebraicDataTypes                    -: Yes
        , DependentTypes                        -: No
        , ForcesImmutability                    -: Quirks
        , ImmutableData                         -: Yes
        , LazinessByDefault                     -: No
        , ParametricModules                     -: Yes
        , ParametricPolymorphism                -: Yes
        , PatternMatching                       -: Yes
        , PatternMatchingAlternatives           -: Yes
        , PatternMatchingVariableIntroduction   -: Yes
        , PolymorphicRecursion                  -: Yes
        , ReferentialTransparency               -: No
        , StaticTyping                          -: Yes
        , TotalityChecking                      -: No
        , UniquenessTypes                       -: No
        , UniversePolymorphism                  -: No
        ]
    ]

data Desc = Desc
    { functional :: Map FunctionalFeature Value
    , supporting :: Map SupportingFeature Value
    }

data FunctionalFeature
    = Closures
    | DownwardsFunargProblem
    | Functions
    | GuaranteedTailCallOptimization
    | LambdaAbstractionSyntax
    | PureFunctions
    | UpwardsFunargProblem
    deriving (Bounded, Enum, Eq, Ord, Show)

data SupportingFeature
    = AdHocPolymorphism
    | AlgebraicDataTypes
    | DependentTypes
    | ForcesImmutability
    | ImmutableData
    | LazinessByDefault
    | ParametricModules
    | ParametricPolymorphism
    | PatternMatching
    | PatternMatchingAlternatives
    | PatternMatchingVariableIntroduction
    | PolymorphicRecursion
    | ReferentialTransparency
    | RowPolymorphism
    | StaticTyping
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
    :   ""
    :   "[You can](https://lispcast.com/fp-in-my-language) write functionally"
    :   "in any language."
    :   ""
    :   "Also, different languages have different functional features."
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
    :   "## Supporting features"
    :   ""
    :   row ("Feature" : keys languages)
    :   ("|---|" ++ concat (replicate (length languages) "---|"))
    :   [ row
            $   wordsSpace (show feature)
            :   [ maybe "" show $ supporting languageFeatures !? feature
                | languageFeatures <- elems languages
                ]
        | feature <- universe
        ]
    ++  ""
    :   "## Scores"
    :   ""
    :   "A well implemented feature counts as 1,"
    :   "a hard-to-use one counts as 0.5."
    :   "Supporting featurues count twice less."
    :   ""
    :   "| Language | Score |"
    :   "|----------|-------|"
    :   [ row [language, show (realToFrac score :: Float)]
        | (score, language) <- sortOn Down
            [ (score, language)
            | (language, Desc{functional, supporting}) <- assocs languages
            , let
                score =
                    sum (value <$> functional) + sum (value <$> supporting) / 2
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
