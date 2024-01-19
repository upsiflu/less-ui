module Main exposing (Msg)

import Control
import Date
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Less exposing (application)
import Less.Ui as Ui
import Less.Ui.Html as Html exposing (Ui)
import Less.Ui.Region exposing (Region(..))
import MultiTool
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


shapeForm =
    Control.simpleForm
        { control = shapeTools.control
        , onSubmit = FormSubmitted
        , onUpdate = ShapeFormUpdated
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


type CounterDelta
    = Increment
    | Decrement


counterControl { makeInnerHtml } =
    Control.create
        { initEmpty = ( 0, Cmd.none )
        , initWith = \output -> ( output, Cmd.none )
        , update =
            \delta state ->
                case delta of
                    Increment ->
                        ( state + 1, Cmd.none )

                    Decrement ->
                        ( state - 1, Cmd.none )
        , view =
            \{ state, name, id, label, class } ->
                [ Html.div [ Attr.class class ]
                    [ Html.label
                        [ Attr.for id ]
                        [ Html.text label ]
                    , Html.div
                        [ Attr.id id
                        , Attr.name name
                        ]
                        [ Html.button
                            [ Attr.type_ "button"
                            , Events.onClick Increment
                            ]
                            [ Html.text "+" ]
                        , Html.div [] [ Html.text <| String.fromInt state ]
                        , Html.button
                            [ Attr.type_ "button"
                            , Events.onClick Decrement
                            ]
                            [ Html.text "-" ]
                        ]
                    ]
                ]
        , subscriptions = \state -> Sub.none
        , parse = Ok
        , label = "Counter"
        }


getCounterForm maybeFormState =
    let
        formState =
            maybeFormState
                |> Maybe.withDefault
                    (Tuple.first (getForm { makeInnerHtml = \_ -> Nothing }).init)

        makeOuterHtml uiState =
            [ (getForm uiState).view formState ]

        getForm makeHtml =
            Control.form
                { control = counterControl makeHtml
                , onUpdate = StringFormUpdated
                , view = Html.fieldset []
                }
    in
    Html.nest
        { combine = makeOuterHtml }


counterForm =
    Control.form
        { control = counterControl { makeInnerHtml = \_ -> Nothing }
        , onUpdate = StringFormUpdated
        , view = Html.fieldset []
        }



---------


getStringControl { makeInnerHtml } =
    Control.create
        { label = "Just a normal String -- but with extra"
        , initEmpty = ( "Nothing yet", Cmd.none )
        , initWith = \pw -> ( pw, Cmd.none )
        , update = \pw state -> ( pw, Cmd.none )
        , view =
            \{ state, id, label, name, class } ->
                let
                    myStaticUi =
                        Html.toggle []
                            { flag = id
                            , inHeader = True
                            , label = [ Html.label [ Attr.for id ] [ Html.text label ] ]
                            }
                            (Ui.singleton
                                [ Html.input
                                    [ Events.onInput identity
                                    , Attr.value state
                                    , Attr.id id
                                    , Attr.class class
                                    , Attr.name name
                                    ]
                                    []
                                , Html.text state
                                ]
                            )
                            ++ (Html.toggle []
                                    { flag = "Fun10"
                                    , inHeader = False
                                    , label = [ Html.text "From GetStringControl to Header.. wheee!" ]
                                    }
                                    []
                                    |> Ui.at Info
                                -- draws an empty fieldset into the Info field, as expected :-)
                                -- Note that the **Form**.view repeats in each Region.
                               )
                            ++ Html.goTo []
                                { destination = ( Just "Hi", Nothing )
                                , inHeader = True
                                , label = [ Html.text "Me, no!" ]
                                }
                                []

                    renderedUi =
                        makeInnerHtml myStaticUi
                            |> Maybe.withDefault []
                in
                renderedUi
                    ++ [ Html.input
                            [ Events.onInput identity
                            , Attr.value state
                            , Attr.id id
                            , Attr.class class
                            , Attr.name name
                            ]
                            []
                       ]
        , subscriptions = \state -> Sub.none
        , parse =
            Ok
        }


getStringForm maybeFormState =
    let
        formState =
            maybeFormState
                |> Maybe.withDefault
                    (Tuple.first (getForm { makeInnerHtml = \_ -> Nothing }).init)

        makeOuterHtml uiState =
            [ (getForm uiState).view formState ]

        getForm makeHtml =
            Control.form
                { control = getStringControl makeHtml
                , onUpdate = StringFormUpdated
                , view = Html.fieldset []
                }
    in
    Html.nest
        { combine = makeOuterHtml }



----


type Msg userDelta
    = UserFormUpdated userDelta


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
        , onUpdate = UserFormUpdated
        , view = Html.fieldset []
        }


tupleIntStringForm =
    Control.simpleForm
        { control = Control.tuple Control.int Control.string
        , onSubmit = FormSubmitted
        , onUpdate = TupleIntStringFormUpdated
        }



---- Application ----
--What about:
{-
   Instead of {innerHtml}->html,

   we can do a Default module like Keyed, but with Form!
   I.e.

-}


textLabel : String -> Ui narrowMsg msg
textLabel str =
    Ui.singleton [ Html.text str ]


main =
    application
        { init =
            ( { userState = userForm.init |> Tuple.first
              }
            , Cmd.none
            )
        , update =
            \msg model ->
                case msg of
                    UserFormUpdated delta ->
                        let
                            ( state, cmd ) =
                                userForm.update delta model.userState
                        in
                        ( { model | userState = state }, cmd )
        , view =
            \model ->
                { body =
                    let
                        moreFun int =
                            Html.toggle []
                                { flag = "Fun" ++ String.fromInt int
                                , inHeader = True
                                , label = [ Html.label [] [ Html.text "＋" ] ]
                                }
                                (Ui.singleton [ tupleIntStringForm.view model.tupleIntStringState ]
                                    ++ (if int > 0 then
                                            moreFun (int - 1)

                                        else
                                            []
                                       )
                                )
                                |> Ui.wrap (Html.Node "ul" [])
                    in
                    textLabel "Header!"
                        ++ Ui.at Scene
                            (textLabel "Scene!")
                        ++ Ui.at
                            Control
                            (Ui.singleton [ userForm.view model.userState ]
                                ++ Ui.singleton [ userForm.view model.userState ]
                                ++ Ui.singleton [ shapeForm.view model.shapeState ]
                            )
                        ++ Ui.at Scene
                            (moreFun 10
                                ++ getStringForm (Just model.stringState)
                                ++ getStringForm (Just model.stringState)
                            )
                        ++ Ui.at Info (textLabel "Info!")
                        ++ Ui.at Info
                            (Html.toggle []
                                { flag = "Fun10", inHeader = False, label = [ Html.text "Hello" ] }
                                []
                            )
                , layout = Html.layout
                , title = "Hello World"
                }
                    |> Less.mapDocument identity
        }
