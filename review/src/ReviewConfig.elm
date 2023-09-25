module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}


import Review.Rule as Rule exposing (Rule)
import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoPrematureLetComputation
import NoSimpleLetBody
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUnsortedCases
import NoUnsortedLetDeclarations
import NoUnsortedRecords
import NoUnsortedTopLevelDeclarations
import Simplify
import ReviewPipelineStyles
import ReviewPipelineStyles.Premade
import OnlyAllSingleUseTypeVarsEndWith_
import CognitiveComplexity
import VariablesBetweenCaseOf.AccessInCases


config : List Rule
config =
    [ NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoExposingEverything.rule
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoSimpleLetBody.rule
    , NoPrematureLetComputation.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults
    , NoUnsortedCases.rule
        (NoUnsortedCases.defaults
            |> NoUnsortedCases.sortListPatternsByLength
        )
    , NoUnsortedLetDeclarations.rule
        (NoUnsortedLetDeclarations.sortLetDeclarations
            |> NoUnsortedLetDeclarations.usedInExpressionFirst
            |> NoUnsortedLetDeclarations.alphabetically
            |> NoUnsortedLetDeclarations.glueHelpersAfter
        )
    , NoUnsortedRecords.rule
        (NoUnsortedRecords.defaults
            |> NoUnsortedRecords.treatAllSubrecordsAsCanonical
            |> NoUnsortedRecords.typecheckAllRecords
            |> NoUnsortedRecords.reportAmbiguousRecordsWithoutFix
        )
    , ReviewPipelineStyles.rule <|
        List.concat
            [ ReviewPipelineStyles.Premade.noSingleLineRightPizza
            , ReviewPipelineStyles.Premade.noPipelinesWithSimpleInputs
            , ReviewPipelineStyles.Premade.noRepeatedParentheticalApplication
            , ReviewPipelineStyles.Premade.noPipelinesWithConfusingNonCommutativeFunctions
            , ReviewPipelineStyles.Premade.noSemanticallyInfixFunctionsInLeftPipelines
            ]
    , OnlyAllSingleUseTypeVarsEndWith_.rule
    , CognitiveComplexity.rule 15
    , VariablesBetweenCaseOf.AccessInCases.forbid
    
    -- Docs
    , Docs.NoMissing.rule
        { document = onlyExposed
        , from = exposedModules
        }
    , Docs.ReviewLinksAndSections.rule
    , Docs.ReviewAtDocs.rule
    , Docs.UpToDateReadmeLinks.rule
    ]
