module Forms.Login exposing
    ( Model
    , Output
    , form
    , init
    )

import Form exposing (Form)
import Form.View as FormView


type alias Model =
    FormView.Model Values


type alias Values =
    { email : String
    , password : String
    }


type alias Output =
    Values


init : Model
init =
    FormView.idle
        { email = "alice@test.org"
        , password = "alicesecret"
        }


form : Form Values Output
form =
    Form.succeed
        (\email password ->
            { email = email
            , password = password
            }
        )
        |> Form.append emailField
        |> Form.append passwordField


emailField : Form { r | email : String } String
emailField =
    Form.textField
        { parser =
            \email ->
                Ok email
        , value = .email
        , update =
            \newValue values ->
                { values | email = newValue }
        , attributes =
            { label = "Email"
            , placeholder = "Email address..."
            }
        , error = always Nothing
        }


passwordField : Form { r | password : String } String
passwordField =
    Form.passwordField
        { parser =
            \password ->
                Ok password
        , value = .password
        , update =
            \newValue values ->
                { values | password = newValue }
        , attributes =
            { label = "Password"
            , placeholder = "Password..."
            }
        , error = always Nothing
        }
