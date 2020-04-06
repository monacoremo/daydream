module UI exposing
    ( borderBottom
    , borderTop
    , buttonAttrs
    , checkIndicator
    , elevation
    , navHeight
    , padding
    , paddingLeft
    , palette
    , panelWidth
    , roundBottom
    , roundTop
    , toggle
    )

import Element exposing (Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


paddingLeft : Int -> Element.Attribute msg
paddingLeft pad =
    Element.paddingEach
        { left = pad
        , top = 0
        , bottom = 0
        , right = 0
        }


borderBottom : Int -> Element.Attribute msg
borderBottom width =
    Border.widthEach
        { top = 0
        , left = 0
        , right = 0
        , bottom = width
        }


borderTop : Int -> Element.Attribute msg
borderTop width =
    Border.widthEach
        { top = width
        , left = 0
        , right = 0
        , bottom = 0
        }


navHeight : Int
navHeight =
    80


padding : Int
padding =
    16


panelWidth : Int
panelWidth =
    16 * 32


buttonAttrs : List (Element.Attribute msg)
buttonAttrs =
    [ Element.padding 8
    , Background.color palette.primary
    , Font.color palette.onPrimary
    , Border.rounded 4
    , elevation 2
    ]


roundTop : Int -> Element.Attribute msg
roundTop radius =
    Border.roundEach
        { topLeft = radius
        , topRight = radius
        , bottomLeft = 0
        , bottomRight = 0
        }


roundBottom : Int -> Element.Attribute msg
roundBottom radius =
    Border.roundEach
        { topLeft = 0
        , topRight = 0
        , bottomLeft = radius
        , bottomRight = radius
        }



-- COLOR


type alias ColorPalette =
    { primary : Color
    , onPrimary : Color
    , primaryVariant : Color
    , secondary : Color
    , onSecondary : Color
    , secondaryVariant : Color
    , background : Color
    , onBackground : Color
    , surface : Color
    , onSurface : Color
    , error : Color
    , onError : Color
    }


palette : ColorPalette
palette =
    { primary = Element.rgb 0 0.835 0
    , onPrimary = Element.rgb 1 1 1
    , primaryVariant = Element.rgb 0.568 1 0
    , secondary = Element.rgb 0.568 1 0
    , onSecondary = Element.rgb 1 1 1
    , secondaryVariant = Element.rgb 0.76 1 0.274
    , background = Element.rgb 0.8 0.8 0.8
    , onBackground = Element.rgb 0 0 0
    , surface = Element.rgb 1 1 1
    , onSurface = Element.rgb 0 0 0
    , error = Element.rgb 1 0 0
    , onError = Element.rgb 1 1 1
    }



-- TOGGLE


toggle : Bool -> Element msg
toggle selected =
    Element.el
        [ Element.width (Element.px 36)
        , Element.height (Element.px 20)
        , Element.centerY
        , Element.padding 2
        , Element.inFront (toggleSlider selected)
        ]
        (toggleTrack selected)


toggleTrack : Bool -> Element msg
toggleTrack selected =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Border.rounded 8
        , Background.color <|
            if selected then
                palette.secondaryVariant

            else
                palette.background
        ]
        Element.none


toggleSlider : Bool -> Element msg
toggleSlider selected =
    Element.el
        [ Element.width (Element.px 20)
        , Element.height (Element.px 20)
        , Border.rounded 10
        , Background.color <|
            if selected then
                palette.secondary

            else
                palette.surface
        , Element.moveRight <|
            if selected then
                16

            else
                0
        , elevation 1
        ]
        Element.none


checkIndicator : Bool -> Element msg
checkIndicator ok =
    Element.el
        [ Element.width (Element.px 8)
        , Element.height (Element.px 8)
        , Element.centerX
        , Element.centerY
        , Background.color
            (if ok then
                Element.rgb 0.1 0.9 0.1

             else
                Element.rgb 0.8 0 0
            )
        , Border.rounded 4
        ]
        Element.none



-- ELEVATION


elevation : Int -> Element.Attribute msg
elevation level =
    Border.shadow
        { offset = ( toFloat level - 1, toFloat level )
        , size = toFloat level
        , blur = 3
        , color = Element.rgba 0 0 0 0.12
        }
