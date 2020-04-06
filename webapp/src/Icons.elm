module Icons exposing
    ( Size(..)
    , dragIndicator
    , icon
    , redo
    , undo
    )

import Element exposing (Element)
import Element.Font as Font exposing (Font)
import Html.Attributes


type Size
    = Small
    | Medium
    | Large
    | XLarge


type alias Icon msg =
    Size -> Element.Color -> Element msg


icon : String -> Icon msg
icon name size color =
    Element.el
        [ Font.family [ materialIconsExternal ]
        , Element.htmlAttribute (Html.Attributes.class "material-icons")
        , Font.size
            (case size of
                Small ->
                    18

                Medium ->
                    24

                Large ->
                    36

                XLarge ->
                    48
            )
        , Font.color color
        , Font.center
        , Element.centerX
        , Element.centerY
        ]
        (Element.text name)


materialIconsExternal : Font
materialIconsExternal =
    Font.external
        { name = "Material Icons"
        , url = "https://fonts.googleapis.com/icon?family=Material+Icons"
        }


dragIndicator : Icon msg
dragIndicator =
    icon "drag_indicator"


undo : Icon msg
undo =
    icon "undo"


redo : Icon msg
redo =
    icon "redo"
