module ParensRule exposing (rule)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "ParensRule" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor (Node range node) =
    case node of
        -- OperatorApplication "||" _ (Node parensRange (ParenthesizedExpression (Node childRange (OperatorApplication "||" _ (Node childParensRange (ParenthesizedExpression (Node grandChildRange (OperatorApplication "||" _ _ _)))) _)))) _ ->
        --     [ Rule.errorWithFix
        --         { message = "Remove these 2 redundant parentheses"
        --         , details = [ "|| is left associative" ]
        --         }
        --         range
        --         [ Fix.removeRange { start = parensRange.start, end = childRange.start }
        --         , Fix.removeRange { start = childParensRange.start, end = grandChildRange.start }
        --         , Fix.removeRange { start = grandChildRange.end, end = childParensRange.end }
        --         , Fix.removeRange { start = childRange.end, end = parensRange.end }
        --         ]
        --     ]
        OperatorApplication "||" _ (Node parensRange (ParenthesizedExpression (Node childRange (OperatorApplication "||" _ _ _)))) _ ->
            [ Rule.errorWithFix
                { message = "Remove these redundant parentheses"
                , details = [ "|| is left associative" ]
                }
                range
                [ Fix.removeRange { start = parensRange.start, end = childRange.start }
                , Fix.removeRange { start = childRange.end, end = parensRange.end }
                ]
            ]

        _ ->
            []
