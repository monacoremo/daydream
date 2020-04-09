module Format exposing
    ( cents
    , percentChange
    , thousands
    )

-- NUMBERS


cents : Float -> String
cents value =
    String.concat
        [ thousands value
        , ","
        , (value * 100)
            |> round
            |> abs
            |> modBy 100
            |> String.fromInt
            |> String.padLeft 2 '0'
        ]


percentChange : Float -> String
percentChange value =
    String.concat
        [ if value > 0 then
            "+"

          else
            ""
        , thousands (100 * value)
        , ","
        , String.fromInt (modBy 10 (floor (1000 * value)))
        , " %"
        ]


thousands : Float -> String
thousands num =
    String.fromFloat num
        |> String.split "."
        |> List.head
        |> Maybe.withDefault ""
        |> splitStringRight 3
        |> String.join "."


splitStringRight : Int -> String -> List String
splitStringRight groupSize str =
    case String.dropRight groupSize str of
        "" ->
            [ str ]

        "-" ->
            [ str ]

        next ->
            splitStringRight groupSize next ++ [ String.right groupSize str ]
