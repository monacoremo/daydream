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
    Parser.s "app"
        </> Parser.oneOf
                [ Parser.map Index Parser.top
                ]


path : Route -> String
path route =
    Builder.absolute
        ("app"
            :: (case route of
                    Index ->
                        [ "" ]

                    Invalid ->
                        [ "" ]
               )
        )
        []


isInternal : Url -> Bool
isInternal url =
    String.startsWith "/app/" url.path
