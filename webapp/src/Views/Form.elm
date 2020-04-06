module Views.Form exposing (layout)

import Element exposing (Attribute, Element)
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Form exposing (Form)
import Form.Error as Error exposing (Error)
import Form.View
    exposing
        ( CheckboxFieldConfig
        , FormConfig
        , FormListConfig
        , FormListItemConfig
        , Model
        , NumberFieldConfig
        , RadioFieldConfig
        , RangeFieldConfig
        , SelectFieldConfig
        , State(..)
        , TextFieldConfig
        , ViewConfig
        )
import Html
import Html.Attributes
import UI


layout : ViewConfig values msg -> Form values msg -> Model values -> Element msg
layout =
    Form.View.custom
        { form = form
        , textField = inputField Input.text
        , emailField = inputField Input.email
        , passwordField = passwordField
        , searchField = inputField Input.search
        , textareaField = textareaField
        , numberField = numberField
        , rangeField = rangeField
        , checkboxField = checkboxField
        , radioField = radioField
        , selectField = selectField
        , group = group
        , section = section
        , formList = formList
        , formListItem = formListItem
        }


formList : FormListConfig msg (Element msg) -> Element msg
formList _ =
    Element.text "formList"


formListItem : FormListItemConfig msg (Element msg) -> Element msg
formListItem _ =
    Element.text "formListItem"


form : FormConfig msg (Element msg) -> Element msg
form { onSubmit, action, loading, state, fields } =
    let
        submitButton =
            Input.button UI.buttonAttrs
                { onPress = onSubmit
                , label =
                    if state == Loading then
                        Element.text loading

                    else
                        Element.text action
                }
    in
    Element.column
        [ Element.spacing 16
        , Element.width Element.fill
        , Element.paddingXY 0 16
        ]
        (fields
            ++ [ case state of
                    Error error ->
                        Element.text error

                    _ ->
                        Element.none
               , submitButton
               ]
        )


inputField :
    (List (Attribute msg)
     ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
     -> Element msg
    )
    -> TextFieldConfig msg
    -> Element msg
inputField input { onChange, onBlur, value, error, showError, attributes } =
    input
        ([] |> withCommonAttrs showError error onBlur)
        { onChange = onChange
        , text = value
        , placeholder = placeholder attributes
        , label = labelAbove (showError && error /= Nothing) attributes
        }


passwordField : TextFieldConfig msg -> Element msg
passwordField { onChange, onBlur, value, error, showError, attributes } =
    Input.currentPassword
        ([] |> withCommonAttrs showError error onBlur)
        { onChange = onChange
        , text = value
        , placeholder = placeholder attributes
        , label = labelAbove (showError && error /= Nothing) attributes
        , show = False
        }


textareaField : TextFieldConfig msg -> Element msg
textareaField { onChange, onBlur, value, error, showError, attributes } =
    Input.multiline
        ([ Element.height Element.shrink ]
            |> withCommonAttrs showError error onBlur
        )
        { onChange = onChange
        , text = value
        , placeholder = placeholder attributes
        , label = labelAbove (showError && error /= Nothing) attributes
        , spellcheck = True
        }


numberField : NumberFieldConfig msg -> Element msg
numberField { onChange, onBlur, value, error, showError, attributes } =
    Input.text
        ([]
            |> withHtmlAttribute Html.Attributes.type_ (Just "number")
            |> withHtmlAttribute (String.fromFloat >> Html.Attributes.step) attributes.step
            |> withHtmlAttribute (String.fromFloat >> Html.Attributes.max) attributes.max
            |> withHtmlAttribute (String.fromFloat >> Html.Attributes.min) attributes.min
            |> withCommonAttrs showError error onBlur
        )
        { onChange = onChange
        , text = value
        , placeholder = placeholder attributes
        , label = labelAbove (showError && error /= Nothing) attributes
        }


rangeField : RangeFieldConfig msg -> Element msg
rangeField { onChange, onBlur, value, error, showError, attributes } =
    Input.text
        ([]
            |> withHtmlAttribute Html.Attributes.type_ (Just "range")
            |> withHtmlAttribute (String.fromFloat >> Html.Attributes.step) (Just attributes.step)
            |> withHtmlAttribute (String.fromFloat >> Html.Attributes.max) attributes.max
            |> withHtmlAttribute (String.fromFloat >> Html.Attributes.min) attributes.min
            |> withCommonAttrs showError error onBlur
        )
        { onChange = fromString String.toFloat value >> onChange
        , text = value |> Maybe.map String.fromFloat |> Maybe.withDefault ""
        , placeholder = Nothing
        , label = labelAbove (showError && error /= Nothing) attributes
        }


checkboxField : CheckboxFieldConfig msg -> Element msg
checkboxField { onChange, onBlur, value, error, showError, attributes } =
    Input.checkbox
        ([ Element.paddingXY 0 16 ]
            |> withCommonAttrs showError error onBlur
        )
        { onChange = onChange
        , icon = Input.defaultCheckbox
        , checked = value
        , label =
            labelAbove (showError && error /= Nothing)
                attributes
        }


radioField : RadioFieldConfig msg -> Element msg
radioField { onChange, onBlur, value, error, showError, attributes } =
    Input.radio
        ([ Element.spacing 16, Element.paddingXY 0 16 ]
            |> withCommonAttrs showError error onBlur
        )
        { onChange = onChange
        , selected = Just value
        , label = labelAbove (showError && error /= Nothing) attributes
        , options =
            List.map
                (\( val, name ) ->
                    Input.option val (Element.text name)
                )
                attributes.options
        }


{-| There is no select field so use a radio instead
-}
selectField : SelectFieldConfig msg -> Element msg
selectField { onChange, onBlur, disabled, value, error, showError, attributes } =
    -- There is no select field so use a radio instead
    radioField
        { onChange = onChange
        , onBlur = onBlur
        , disabled = disabled
        , value = value
        , error = error
        , showError = showError
        , attributes =
            { label = attributes.label
            , options = attributes.options
            }
        }


group : List (Element msg) -> Element msg
group =
    Element.row [ Element.spacing 12 ]


section : String -> List (Element msg) -> Element msg
section title fields =
    Element.column
        [ Border.solid
        , Border.width 1
        , Element.padding 16
        , Element.width Element.fill
        , Element.inFront
            (Element.el
                [ Element.moveUp 16
                , Element.moveRight 16
                , Element.padding 8
                , Element.width Element.shrink
                ]
                (Element.text title)
            )
        ]
        fields


errorToString : Error -> String
errorToString error =
    case error of
        Error.RequiredFieldIsEmpty ->
            "This field is required"

        Error.ValidationFailed validationError ->
            validationError

        Error.External _ ->
            "external"



-- Common Elements


placeholder : { r | placeholder : String } -> Maybe (Input.Placeholder msg)
placeholder attributes =
    Just (Input.placeholder [] (Element.text attributes.placeholder))


labelAbove : Bool -> { r | label : String } -> Input.Label msg
labelAbove _ attributes =
    Input.labelAbove [] (Element.text attributes.label)


fieldError : String -> Element msg
fieldError error =
    Element.text error



-- Helpers


fromString : (String -> Maybe a) -> Maybe a -> String -> Maybe a
fromString parse currentValue input =
    if String.isEmpty input then
        Nothing

    else
        parse input
            |> Maybe.map Just
            |> Maybe.withDefault currentValue


withCommonAttrs :
    Bool
    -> Maybe Error
    -> Maybe msg
    -> List (Attribute msg)
    -> List (Attribute msg)
withCommonAttrs showError error onBlur attrs =
    attrs
        |> when showError
            (Element.below
                (error
                    |> Maybe.map errorToString
                    |> Maybe.map fieldError
                    |> Maybe.withDefault Element.none
                )
            )
        |> whenJust onBlur Events.onLoseFocus


when : Bool -> Attribute msg -> List (Attribute msg) -> List (Attribute msg)
when test attr attrs =
    if test then
        attr :: attrs

    else
        attrs


whenJust :
    Maybe a
    -> (a -> Attribute msg)
    -> List (Attribute msg)
    -> List (Attribute msg)
whenJust maybeValue toAttribute attrs =
    Maybe.map (toAttribute >> (\attr -> attr :: attrs)) maybeValue
        |> Maybe.withDefault attrs


withHtmlAttribute :
    (a -> Html.Attribute msg)
    -> Maybe a
    -> List (Attribute msg)
    -> List (Attribute msg)
withHtmlAttribute toAttribute maybeValue attrs =
    Maybe.map (toAttribute >> Element.htmlAttribute >> (\attr -> attr :: attrs)) maybeValue
        |> Maybe.withDefault attrs
