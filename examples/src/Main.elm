module Main exposing (Msg)

import Browser
import Control exposing (Control)
import Html exposing (Html)
import Html.Attributes as Attr
import Restrictive exposing (Application, application)
import Restrictive.Layout as Layout
import Restrictive.Layout.Html.Keyed exposing (Wrapper(..))
import Restrictive.Layout.Region exposing (Region(..))
import Restrictive.Ui as Ui exposing (at, with, wrap)


type Msg delta0 delta1
    = Form0Updated delta0
    | Form1Updated delta1
    | FormSubmitted


type alias User =
    { name : String
    , age : Int
    , role : Role
    }


userControl =
    Control.record User
        |> Control.field .name Control.string
        |> Control.field .age Control.int
        |> Control.field .role roleControl
        |> Control.endRecord


type Role
    = Regular
    | AdminLevel Int String


roleControl =
    Control.customType
        (\regular adminLevel tag ->
            case tag of
                Regular ->
                    regular

                AdminLevel level string ->
                    adminLevel level string
        )
        |> Control.tag0 "Regular" Regular
        |> Control.tag2 "Admin Level" AdminLevel Control.int Control.string
        |> Control.endCustomType
        |> Control.label "User"


userForm =
    Control.form
        { control = userControl
        , onUpdate = Form0Updated
        , view = \children -> Html.fieldset [] children
        }


funForm =
    Control.simpleForm
        { control = Control.tuple Control.int Control.string
        , onSubmit = FormSubmitted
        , onUpdate = Form1Updated
        }



---- Application ----


textLabel : String -> Ui.Ui region (List ( String, Html msg )) attribute wrapper
textLabel str =
    Ui.singleton [ ( str, Html.text str ) ]


main =
    application
        { init = ( { funState = funForm.init |> Tuple.first, state = userForm.init |> Tuple.first }, Cmd.none )
        , update =
            \msg model ->
                case msg of
                    Form0Updated delta ->
                        let
                            ( state, cmd ) =
                                userForm.update delta model.state
                        in
                        ( { model | state = state }, cmd )

                    Form1Updated delta ->
                        let
                            ( state, cmd ) =
                                funForm.update delta model.funState
                        in
                        ( { model | funState = state }, cmd )

                    _ ->
                        ( model, Cmd.none )
        , view =
            \model ->
                { body =
                    at Scene
                        (textLabel "Scene!")
                        ++ at
                            Control
                            (Ui.singleton
                                [ ( "Form", userForm.view model.state )
                                , ( "Fun", funForm.view model.funState )
                                ]
                            )
                        ++ at Info (textLabel "Info!")
                , layout = Restrictive.Layout.Html.Keyed.default
                , title = "Hello World"
                }
        }
