module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Derive
import Docs.NoMissing
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import EqualsCaseable
import HtmlToElm
import LimitAliasedRecordSize
import NewlineRule
import NoBooleanCase
import NoBrokenParserFunctions
import NoCatchAllForSpecificRemainingPatterns
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoDeprecated
import NoDuplicatePorts
import NoExposingEverything
import NoImportingEverything
import NoInternalImports
import NoInvalidRGBValues
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoModuleOnExposedNames
import NoPrematureLetComputation
import NoRecursiveUpdate
import NoRedundantlyQualifiedType
import NoSimpleLetBody
import NoSinglePatternCase
import NoUnnecessaryTrailingUnderscore
import NoUnoptimizedRecursion
import NoUnsafeDivision
import NoUnsafePorts
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import ParensRule
import Review.Documentation.CodeSnippet
import Review.Rule as Rule exposing (Rule)
import ReviewPipelineStyles
import ReviewPipelineStyles.Fixes
import ReviewPipelineStyles.Predicates
import ReviewPipelineStyles.Premade
import Simplify
import UseCamelCase
import Validate.Regexes
import VariablesBetweenCaseOf.AccessInCases


config : List Rule
config =
    [ Docs.NoMissing.rule
        { document = Docs.NoMissing.onlyExposed
        , from = Docs.NoMissing.exposedModules
        }
    , Derive.rule True []
    , Docs.ReviewLinksAndSections.rule
    , Docs.ReviewAtDocs.rule
    , Docs.UpToDateReadmeLinks.rule
    , HtmlToElm.rule
    , LimitAliasedRecordSize.rule (20 |> LimitAliasedRecordSize.maxRecordSize)
    , NoBooleanCase.rule
    , NoBrokenParserFunctions.rule
    , NoConfusingPrefixOperator.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoDeprecated.rule NoDeprecated.defaults
    , NoDuplicatePorts.rule
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoInternalImports.rule []
    , NoInvalidRGBValues.rule
    , NoMissingSubscriptionsCall.rule
    , NoMissingTypeAnnotation.rule

    -- , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule
    , NoSimpleLetBody.rule
    , NoModuleOnExposedNames.rule
    , NoPrematureLetComputation.rule
    , NoRecursiveUpdate.rule

    -- , NoRedundantlyQualifiedType.rule
    , NoSinglePatternCase.rule NoSinglePatternCase.fixInArgument
    , NoUnnecessaryTrailingUnderscore.rule
    , NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
    , NoUnsafeDivision.rule
    , NoUnsafePorts.rule NoUnsafePorts.onlyIncomingPorts
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForDirectories [ "Gen/" ]
        |> Rule.ignoreErrorsForFiles [ "Categories.elm" ]

    -- , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , ReviewPipelineStyles.rule pipelineConfig |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , Simplify.rule Simplify.defaults
    , ParensRule.rule
    , NewlineRule.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , UseCamelCase.rule UseCamelCase.default
    , Validate.Regexes.rule
    ]
        |> List.map (Rule.ignoreErrorsForFiles [ "src/OldUnicode.elm" ])


pipelineConfig : List (ReviewPipelineStyles.PipelineRule ())
pipelineConfig =
    List.concat
        [ [ ReviewPipelineStyles.forbid ReviewPipelineStyles.leftPizzaPipelines
                |> ReviewPipelineStyles.andTryToFixThemBy ReviewPipelineStyles.Fixes.convertingToParentheticalApplication
                |> ReviewPipelineStyles.andCallThem "forbidden <| pipeline"
          , ReviewPipelineStyles.forbid ReviewPipelineStyles.leftCompositionPipelines
                |> ReviewPipelineStyles.andCallThem "forbidden << composition"
          , ReviewPipelineStyles.forbid ReviewPipelineStyles.rightCompositionPipelines
                |> ReviewPipelineStyles.andCallThem "forbidden >> composition"
          , ReviewPipelineStyles.forbid ReviewPipelineStyles.parentheticalApplicationPipelines
                |> ReviewPipelineStyles.that
                    (ReviewPipelineStyles.Predicates.haveMoreStepsThan 2
                        |> ReviewPipelineStyles.Predicates.and
                            (ReviewPipelineStyles.Predicates.doNot
                                (ReviewPipelineStyles.Predicates.haveAParentNotSeparatedBy
                                    [ ReviewPipelineStyles.Predicates.aLetBlock
                                    , ReviewPipelineStyles.Predicates.aLambdaFunction
                                    , ReviewPipelineStyles.Predicates.aFlowControlStructure
                                    , ReviewPipelineStyles.Predicates.aDataStructure
                                    ]
                                )
                            )
                    )
                |> ReviewPipelineStyles.andTryToFixThemBy ReviewPipelineStyles.Fixes.convertingToRightPizza
                |> ReviewPipelineStyles.andCallThem "parenthetical application with several steps"
          ]
        , ReviewPipelineStyles.Premade.noPipelinesWithConfusingNonCommutativeFunctions
        , ReviewPipelineStyles.Premade.noSemanticallyInfixFunctionsInLeftPipelines
        ]
