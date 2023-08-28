module Main exposing (Msg)

import Control exposing (Control)
import Date
import Html
import Html.Attributes as Attr
import MultiTool
import Restrictive exposing (application)
import Restrictive.Layout.Html as Html exposing (Ui, Wrapper(..))
import Restrictive.Layout.Region as Region exposing (Region(..))
import Restrictive.Ui as Ui
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
        |> tools.tag1 "●" Circle tools.int
        |> tools.tag3 "▲" Triangle tools.int tools.int tools.int
        |> tools.tag2 "▬" Rectangle tools.int tools.int
        |> tools.endCustom


shapeTools =
    tools.build shapeSpec


newForm =
    Control.simpleForm
        { control = shapeTools.control
        , onSubmit = FormSubmitted
        , onUpdate = Form2Updated
        }


dateControl =
    Control.create
        { label = "Date of birth"
        , initEmpty = ( "1970-01-01", Cmd.none )
        , initWith = \date -> ( Date.format "yyyy-MM-dd" date, Cmd.none )
        , update = \delta state -> ( delta, Cmd.none )
        , view =
            \{ state, id, label, name, class } ->
                [ Html.label [ Attr.for id ] [ Html.text label ]
                , Html.input
                    [ Attr.type_ "date"
                    , Attr.value state
                    , Attr.id id
                    , Attr.class class
                    , Attr.name name
                    ]
                    []
                , Html.label [ Attr.for id ] [ Html.text "in\u{00A0}your\u{00A0}timezone" ]
                ]
        , subscriptions = \state -> Sub.none
        , parse =
            \state ->
                case Date.fromIsoString state of
                    Ok date ->
                        Ok date

                    Err error ->
                        Err [ error ]
        }


passwordControl =
    Control.create
        { label = "Password"
        , initEmpty = ( "", Cmd.none )
        , initWith = \pw -> ( pw, Cmd.none )
        , update = \pw state -> ( pw, Cmd.none )
        , view =
            \{ state, id, label, name, class } ->
                [ Html.label [ Attr.for id ] [ Html.text label ]
                , Html.input
                    [ Attr.type_ "password"
                    , Attr.value state
                    , Attr.id id
                    , Attr.class class
                    , Attr.name name
                    ]
                    []
                ]
        , subscriptions = \state -> Sub.none
        , parse =
            \state ->
                case Date.fromIsoString state of
                    Ok date ->
                        Ok state

                    Err error ->
                        Err [ error ]
        }


headerForm formState =
    let
        makeHeaderControl :
            { makeInnerHtml :
                Ui.Ui
                    Region
                    Ui.StatelessHtml
                    Ui.StatelessAttribute
                    Ui.StatelessWrapper
                -> List (Html.Html Never)
            }
            -> Control String String String
        makeHeaderControl { makeInnerHtml } =
            Control.create
                { label = "Password"
                , initEmpty = ( "", Cmd.none )
                , initWith = \pw -> ( pw, Cmd.none )
                , update = \pw state -> ( pw, Cmd.none )
                , view =
                    \{ state, id, label, name, class } ->
                        let
                            myUi :
                                Ui.Ui
                                    Region
                                    Ui.StatelessHtml
                                    Ui.StatelessAttribute
                                    Ui.StatelessWrapper
                            myUi =
                                Ui.toggle []
                                    { flag = id
                                    , isInline = True
                                    , label = [ Html.label [ Attr.for id ] [ Html.text label ] ]
                                    }
                                    (Ui.singleton
                                        [ Html.input
                                            [ Attr.type_ "password"
                                            , Attr.value state
                                            , Attr.id id
                                            , Attr.class class
                                            , Attr.name name
                                            ]
                                            []
                                        ]
                                    )
                                    ++ (Ui.toggle []
                                            { flag = "Fun10"
                                            , isInline = False
                                            , label = [ Html.text "I want to be in the Hader!" ]
                                            }
                                            []
                                            |> Ui.at Info
                                       )
                                    ++ Ui.goTo []
                                        { destination = ( Just "Hi", Nothing )
                                        , isInline = False
                                        , label = [ Html.text "Me too!" ]
                                        }
                                        []
                        in
                        myUi
                            |> Ui.wrap (Ui.Node "ol" [])
                            |> makeInnerHtml
                            |> List.map (Html.map never)
                , subscriptions = \state -> Sub.none
                , parse =
                    \state ->
                        case Date.fromIsoString state of
                            Ok date ->
                                Ok state

                            Err error ->
                                Err [ error ]
                }

        myHeaderForm uiState =
            Control.form
                { control = makeHeaderControl uiState
                , onUpdate = StringUpdated
                , view = Html.fieldset []
                }

        myOuterFormWithoutFormState =
            Ui.toggle []
                { flag = "Fun10"
                , isInline = False
                , label = [ Html.text "I want to be in the Hader!" ]
                }
                []
                |> Ui.at Info
    in
    myOuterFormWithoutFormState
        ++ Ui.stateful
            [ Region.Scene, Region.Info, Region.Control ]
            Html.passiveLayout
            { makeOuterHtml =
                \makeInnerHtml ->
                    [ (myHeaderForm makeInnerHtml).view formState
                    ]
            }


type alias StatelessHtml =
    List (Html.Html Never)


type alias StatelessAttribute =
    Html.Attribute Never


stringForm =
    Control.form
        { control = Control.string
        , onUpdate = StringUpdated
        , view = Html.fieldset []
        }



----


type Msg delta0 delta1 delta2 deltaString
    = Form0Updated delta0
    | Form1Updated delta1
    | Form2Updated delta2
    | StringUpdated deltaString
    | FormSubmitted


type alias User =
    { name : String
    , birth : Date.Date
    , role : Role
    , shapes : List Shape
    }


userControl =
    Control.record User
        |> Control.field .name (Control.string |> Control.label "Name")
        |> Control.field .birth dateControl
        |> Control.field .role roleControl
        |> Control.field .shapes (Control.list (shapeTools.control |> Control.label "") |> Control.label "Shapes")
        |> Control.endRecord


type Role
    = Regular
    | AdminLevel Int String String


roleControl =
    Control.customType
        (\regular adminLevel tag ->
            case tag of
                Regular ->
                    regular

                AdminLevel level string2 string3 ->
                    adminLevel level string2 string3
        )
        |> Control.tag0 "Regular" Regular
        |> Control.tag3 "Admin..." AdminLevel (Control.int |> Control.label "Level") (passwordControl |> Control.label "Admin\u{00A0}Password") (Control.string |> Control.label "Admin Password\u{00A0}Hint")
        |> Control.endCustomType
        |> Control.label "User"


userForm =
    Control.form
        { control = userControl
        , onUpdate = Form0Updated
        , view = Html.fieldset []
        }


funForm =
    Control.simpleForm
        { control = Control.tuple Control.int Control.string
        , onSubmit = FormSubmitted
        , onUpdate = Form1Updated
        }



---- Application ----
--What about:
{-
   Instead of {innerHtml}->html,

   we can do a Default module like Keyed, but with Form!
   I.e.

-}


textLabel : String -> Ui msg
textLabel str =
    Ui.singleton [ Html.text str ]


main =
    application
        { init =
            ( { funState = funForm.init |> Tuple.first
              , state = userForm.init |> Tuple.first
              , newState = newForm.init |> Tuple.first
              , stringState = stringForm.init |> Tuple.first
              }
            , Cmd.none
            )
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
                    let
                        moreFun int =
                            Ui.toggle []
                                { flag = "Fun" ++ String.fromInt int
                                , isInline = True
                                , label = [ Html.label [] [ Html.text "＋" ] ]
                                }
                                (Ui.singleton [ funForm.view model.funState ]
                                    ++ (if int > 0 then
                                            moreFun (int - 1)

                                        else
                                            []
                                       )
                                )
                                |> Ui.wrap (Node "ul" [])
                    in
                    textLabel "Header!"
                        ++ Ui.at Scene
                            (textLabel "Scene!")
                        ++ Ui.at
                            Control
                            (Ui.singleton
                                [ userForm.view model.state ]
                                ++ Ui.singleton [ newForm.view model.newState ]
                            )
                        ++ Ui.at Scene
                            (moreFun 10
                                ++ headerForm model.stringState
                            )
                        ++ Ui.at Info (textLabel "Info!")
                        ++ Ui.at Info
                            (Ui.toggle []
                                { flag = "Fun10", isInline = False, label = [ Html.text "Hello" ] }
                                []
                            )
                , layout = Html.layout
                , title = "Hello World"
                }
                    |> Restrictive.mapDocument Html.toHtml
        }
