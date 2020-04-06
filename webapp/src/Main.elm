module Main exposing (main)

import Api
import Api.Auth
import Browser
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Form
import Form.View as FormView
import Forms.Login as LoginForm
import Html
import Html.Attributes as HtmlAttrs
import Html.Events as HtmlEvents
import Http
import Icons
import Json.Decode as Decode
import Routes exposing (Route)
import Time
import Types.UserInfo exposing (UserInfo)
import UI exposing (palette)
import Url exposing (Url)
import Views.Form


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
    { route : Route
    , navKey : Nav.Key
    , state : State
    }


type State
    = Initializing
    | LoggedOut (FormState LoginForm.Model)
    | LoggedIn UserModel


type alias UserModel =
    { userInfo : UserInfo
    }


type FormState form
    = FormOk form
    | FormSubmitted form
    | FormErrored form String


formStateForm : FormState form -> form
formStateForm state =
    case state of
        FormOk form ->
            form

        FormSubmitted form ->
            form

        FormErrored form _ ->
            form



-- INIT


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    ( { route = Routes.parse url
      , navKey = navKey
      , state = Initializing
      }
    , Api.Auth.userInfo InitGotUserInfo
    )


initUserModel : UserInfo -> UserModel
initUserModel userInfo =
    { userInfo = userInfo
    }



-- MSG


type Msg
    = GotLoggedIn (Result Http.Error ())
    | InitGotUserInfo (Result Http.Error UserInfo)
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | LoggedInMsg LoggedInMsg
    | Logout
    | GotLogout (Result Http.Error ())
    | LoginFormChanged LoginForm.Model
    | LoginFormSubmitted LoginForm.Output
    | CheckAuth
    | AuthChecked (Result Http.Error ())


type LoggedInMsg
    = LoggedInMsgBla



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitGotUserInfo (Ok userInfo) ->
            let
                ( userModel, cmd ) =
                    initUserModel userInfo
                        |> routeLoad model.route
            in
            ( { model | state = LoggedIn userModel }
            , cmd
            )

        InitGotUserInfo (Err _) ->
            ( { model | state = LoggedOut (FormOk LoginForm.init) }, Cmd.none )

        GotLoggedIn (Ok ()) ->
            let
                ( userModel, cmd ) =
                    initUserModel
                        { userid = 1
                        , name = "Alice"
                        , email = "alice@test.org"
                        , role = "webuser"
                        }
                        |> routeLoad model.route
            in
            ( { model | state = LoggedIn userModel }
            , cmd
            )

        GotLoggedIn (Err _) ->
            ( { model | state = LoggedOut (FormErrored LoginForm.init "login failed") }
            , Cmd.none
            )

        UrlChanged url ->
            if Routes.isInternal url then
                updateRoute (Routes.parse url) model

            else
                ( model, Nav.load url.path )

        UrlRequested (Browser.Internal url) ->
            ( model, Nav.pushUrl model.navKey (Url.toString url) )

        UrlRequested (Browser.External href) ->
            ( model, Nav.load href )

        LoggedInMsg limsg ->
            case model.state of
                Initializing ->
                    ( model, Cmd.none )

                LoggedOut _ ->
                    ( model, Cmd.none )

                LoggedIn stateModel ->
                    let
                        ( newModel, cmd ) =
                            updateLoggedIn model.navKey model.route limsg stateModel
                    in
                    ( { model | state = LoggedIn newModel }, cmd )

        Logout ->
            ( { model | state = LoggedOut (FormOk LoginForm.init) }
            , Api.Auth.logout GotLogout
            )

        GotLogout _ ->
            ( { model | state = LoggedOut (FormOk LoginForm.init) }
            , Cmd.none
            )

        LoginFormChanged formModel ->
            ( { model | state = LoggedOut (FormOk formModel) }, Cmd.none )

        LoginFormSubmitted values ->
            ( { model | state = LoggedOut (FormSubmitted LoginForm.init) }
            , Api.Auth.login values.email values.password GotLoggedIn
            )

        CheckAuth ->
            ( model, Api.Auth.check AuthChecked )

        AuthChecked (Err (Http.BadStatus 401)) ->
            ( { model | state = LoggedOut (FormOk LoginForm.init) }
            , Cmd.none
            )

        AuthChecked (Err (Http.BadStatus 403)) ->
            ( { model | state = LoggedOut (FormOk LoginForm.init) }
            , Cmd.none
            )

        AuthChecked _ ->
            ( model, Cmd.none )


updateLoggedIn : Nav.Key -> Route -> LoggedInMsg -> UserModel -> ( UserModel, Cmd Msg )
updateLoggedIn navKey route msg model =
    ( model, Cmd.none )


updateRoute : Routes.Route -> Model -> ( Model, Cmd Msg )
updateRoute route model =
    case model.state of
        Initializing ->
            ( { model | route = route }, Cmd.none )

        LoggedOut _ ->
            ( { model | route = route }, Cmd.none )

        LoggedIn userModel ->
            let
                ( newUserModel, cmd ) =
                    routeLoad route userModel
            in
            ( { model
                | route = route
                , state = LoggedIn newUserModel
              }
            , cmd
            )


routeLoad : Route -> UserModel -> ( UserModel, Cmd Msg )
routeLoad route model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title =
        title model
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Background.color palette.background
            , Font.family [ Font.typeface "Roboto", Font.sansSerif ]
            , Font.size 18
            ]
            (case model.state of
                Initializing ->
                    Element.text "Initializing..."

                LoggedOut formState ->
                    viewLogin formState

                LoggedIn userModel ->
                    viewMain model.route userModel
            )
        ]
    }


title : Model -> String
title model =
    "Daydream - "
        ++ (case model.state of
                Initializing ->
                    "..."

                LoggedOut _ ->
                    "Login"

                LoggedIn _ ->
                    "Logged in"
           )


viewNav : UserModel -> Element Msg
viewNav model =
    Element.row
        [ Element.width Element.fill
        , Background.color palette.surface
        , UI.elevation 2
        ]
        [ Element.link
            [ Element.padding 24 ]
            { url = Routes.path Routes.Index
            , label = Element.text "Daydream"
            }
        , Element.link
            [ Element.alignRight
            , Element.padding 24
            ]
            { url = "/docs/"
            , label = Element.text "Documentation"
            }
        , Element.el [ Element.alignRight ] (viewUserInfo model.userInfo)
        ]


viewUserInfo : UserInfo -> Element Msg
viewUserInfo userInfo =
    Element.el [ Events.onClick Logout ]
        (Element.text userInfo.email)


viewMain : Routes.Route -> UserModel -> Element Msg
viewMain route model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewNav model
        , case route of
            Routes.Index ->
                viewIndex model

            Routes.Invalid ->
                Element.text "invalid url"
        ]


viewLogin : FormState LoginForm.Model -> Element Msg
viewLogin formState =
    Element.column
        [ Element.paddingEach { top = 16, right = 16, bottom = 0, left = 16 }
        , Background.color palette.surface
        , Element.width (Element.px 400)
        , Element.centerX
        , Element.centerY
        ]
        [ Element.el [ Element.width Element.fill, Font.bold, Font.center ]
            (Element.text "Daydream login")
        , case formState of
            FormOk form ->
                viewLoginForm form

            FormSubmitted _ ->
                Element.text "submitted..."

            FormErrored form _ ->
                Element.column [] [ Element.text "error", viewLoginForm form ]
        ]


viewLoginForm : LoginForm.Model -> Element Msg
viewLoginForm form =
    Views.Form.layout
        { onChange = LoginFormChanged
        , action = "Login"
        , loading = "Logging in..."
        , validation = FormView.ValidateOnSubmit
        }
        (Form.map LoginFormSubmitted LoginForm.form)
        form


viewIndex : UserModel -> Element msg
viewIndex model =
    Element.text "index"



-- LOADING


type Loading selection data
    = NotRequested
    | Loading selection
    | Loaded selection data
    | Errored selection Http.Error


type alias LoadingResult selection data =
    Result Http.Error ( selection, data )


load :
    Api.Request data msg
    -> (LoadingResult selection data -> msg)
    -> selection
    -> Cmd msg
load request tagger selection =
    request (Result.map (\r -> ( selection, r )) >> tagger)


loadResult : LoadingResult selection data -> Loading selection data -> Loading selection data
loadResult result loading =
    case loading of
        NotRequested ->
            NotRequested

        Loading selection ->
            case result of
                Ok ( loadedSelection, data ) ->
                    if selection == loadedSelection then
                        Loaded selection data

                    else
                        loading

                Err error ->
                    Errored selection error

        Loaded _ _ ->
            loading

        Errored _ _ ->
            loading


viewLoading : (data -> Element msg) -> Loading selection data -> Element msg
viewLoading viewLoaded loading =
    case loading of
        NotRequested ->
            Element.text "not requested"

        Loading _ ->
            Element.text "loading..."

        Loaded _ loaded ->
            viewLoaded loaded

        Errored _ _ ->
            Element.text "error"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Initializing ->
            Sub.none

        LoggedOut _ ->
            Sub.none

        LoggedIn userModel ->
            Sub.batch
                [ Time.every (60 * 1000) (always CheckAuth)
                ]
