module Routes exposing (Route(..), isInternal, parse, path)

import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Index
    | Invalid


parse : Url -> Route
parse url =
    Parser.parse parser url |> Maybe.withDefault Invalid


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Index Parser.top
        , Parser.s "app"
            </> Parser.oneOf
                    [ Parser.map Index Parser.top
                    ]
        ]


path : Route -> String
path route =
    case route of
        Index ->
            Builder.absolute [ "" ] []

        Invalid ->
            Builder.absolute [ "" ] []


isInternal : Url -> Bool
isInternal url =
    (url.path == "/")
        || String.startsWith "/app/" url.path
