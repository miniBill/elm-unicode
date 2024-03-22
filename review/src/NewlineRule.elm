module NewlineRule exposing (rule)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NewlineRule" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor (Node range node) =
    case node of
        OperatorApplication "||" _ (Node lrange _) (Node rrange _) ->
            if range.start.row /= range.end.row then
                [ Rule.errorWithFix
                    { message = "Make ||s be on a single line"
                    , details = [ "This makes the code smaller" ]
                    }
                    range
                    [ Fix.replaceRangeBy { start = lrange.end, end = rrange.start } " || " ]
                ]

            else
                []

        Application nodes ->
            if
                List.minimum (List.map (\(Node childRange _) -> childRange.start.row) nodes)
                    /= List.maximum (List.map (\(Node childRange _) -> childRange.end.row) nodes)
            then
                [ Rule.error
                    --WithFix
                    { message = "Make applications be on a single line"
                    , details = [ "This makes the code smaller" ]
                    }
                    range

                -- [ Fix.replaceRangeBy { start = lrange.end, end = rrange.start } " || " ]
                ]

            else
                []

        _ ->
            []
