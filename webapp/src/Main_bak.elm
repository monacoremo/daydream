module Main exposing (main)

--import Api.Events

import Browser
import Browser.Navigation as Nav
import Element
import Postgrest
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { route : String
    , navKey : Nav.Key
    , state : State
    }


type State
    = Initializing
    | LoggedOut LoggedOutState
    | LoggedIn LoggedInState


type alias LoggedOutState =
    { registrationForm : ()
    , loginForm : ()
    , view : LoggedOutView
    }


type LoggedOutView
    = LoginView
    | RegistrationView


type alias LoggedInState =
    ()


type alias UserModel =
    ()



-- INIT


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    ( { route = Url.toString url
      , navKey = navKey
      , state = Initializing
      }
    , Cmd.none
    )



-- MSG


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            ( { model | route = Url.toString url }, Cmd.none )

        UrlRequested (Browser.Internal url) ->
            ( model, Nav.pushUrl model.navKey (Url.toString url) )

        UrlRequested (Browser.External href) ->
            ( model, Nav.load href )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Daydream - Login"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            (Element.text "Login here...")
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
