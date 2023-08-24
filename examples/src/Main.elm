module Main exposing (Msg)

import Browser
import Control exposing (Control)
import Html exposing (Html)
import Html.Attributes as Attr
import MultiTool
import Restrictive exposing (Application, application)
import Restrictive.Layout as Layout
import Restrictive.Layout.Html.Keyed exposing (Wrapper(..))
import Restrictive.Layout.Region exposing (Region(..))
import Restrictive.Ui as Ui exposing (at, with, wrap)
import Tools.Control



----
--Tools


type alias Tools control =
    { control : control
    }


tools =
    MultiTool.define Tools Tools
        |> MultiTool.add .control Tools.Control.interface
        |> MultiTool.end


type Shape
    = Circle Int
    | Triangle Int Int Int
    | Rectangle Int Int


shapes =
    [ Circle 1
    , Rectangle 1 2
    , Triangle 4 5 6
    ]


shapeSpec =
    let
        match circle triangle rectangle tag =
            case tag of
                Circle radius ->
                    circle radius

                Triangle side1 side2 side3 ->
                    triangle side1 side2 side3

                Rectangle width height ->
                    rectangle width height
    in
    tools.custom
        { control = match
        }
        |> tools.tag1 "Circle" Circle tools.int
        |> tools.tag3 "Triangle" Triangle tools.int tools.int tools.int
        |> tools.tag2 "Rectangle" Rectangle tools.int tools.int
        |> tools.endCustom


shapeTools =
    tools.build shapeSpec


newForm =
    Control.simpleForm
        { onUpdate = Form2Updated
        , onSubmit = FormSubmitted
        , control = shapeTools.control
        }



----


type Msg delta0 delta1 delta2
    = Form0Updated delta0
    | Form1Updated delta1
    | Form2Updated delta2
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
        { init = ( { funState = funForm.init |> Tuple.first, state = userForm.init |> Tuple.first, newState = newForm.init |> Tuple.first }, Cmd.none )
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

                    Form2Updated delta ->
                        let
                            ( state, cmd ) =
                                newForm.update delta model.newState
                        in
                        ( { model | newState = state }, cmd )

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
                                [ ( "Form", userForm.view model.state ) ]
                                ++ (Ui.toggle []
                                        { flag = "Fun"
                                        , isInline = True
                                        , label = [ ( "button", Html.label [] [ Html.text "Fun..." ] ) ]
                                        }
                                        |> with (Ui.singleton [ ( "Fun", funForm.view model.funState ) ])
                                   )
                                ++ Ui.singleton [ ( "New", newForm.view model.newState ) ]
                            )
                        ++ at Info (textLabel "Info!")
                , layout = Restrictive.Layout.Html.Keyed.default
                , title = "Hello World"
                }
        }
