module Main exposing (Model, Msg, main)

import Browser
import Control
import Html exposing (Html)
import Html.Attributes as Attr
import Restrictive exposing (Application, application)
import Restrictive.Layout as Layout
import Restrictive.Layout.Region exposing (Region(..))
import Restrictive.Ui as Ui


{-| Using [edkelly303/elm-any-type-forms]()
-}
type alias Model =
    { state :
        Control.State
            ( Control.State String
            , ( Control.State String
              , ( Control.State
                    ( Control.State ()
                    , ( Control.State
                            ( Control.State String, Control.End )
                      , Control.End
                      )
                    )
                , Control.End
                )
              )
            )
    }


type Msg
    = FormUpdated
        (Control.Delta
            ( Control.Delta String
            , ( Control.Delta String
              , ( Control.Delta
                    ( Control.Delta ()
                    , ( Control.Delta ( Control.Delta String, Control.End )
                      , Control.End
                      )
                    )
                , Control.End
                )
              )
            )
        )
    | FormSubmitted


type alias User =
    { name : String
    , age : Int
    , role : Role
    }


userControl =
    Control.record
        (\name age role ->
            { name = name
            , age = age
            , role = role
            }
        )
        |> Control.field "Name" .name Control.string
        |> Control.field "Age" .age Control.int
        |> Control.field "Role" .role roleControl
        |> Control.end


type Role
    = Regular
    | AdminLevel Int


roleControl =
    Control.customType
        (\regular adminLevel tag ->
            case tag of
                Regular ->
                    regular

                AdminLevel level ->
                    adminLevel level
        )
        |> Control.tag0 "Regular" Regular
        |> Control.tag1 "Admin Level" AdminLevel Control.int
        |> Control.end


userForm =
    Control.toForm
        "Let's make a User"
        FormUpdated
        FormSubmitted
        userControl



---- Application ----


type alias KeyedHtmlWrapper =
    List ( String, Html Msg ) -> List ( String, Html Msg )


type alias Ui =
    Ui.Ui Region KeyedHtmlWrapper ( String, Html Msg )


type alias Document =
    Restrictive.Document Region KeyedHtmlWrapper ( String, Html Msg )


main : Application Model Msg
main =
    application
        { init = ( { state = userForm.init }, Cmd.none )
        , update =
            \msg model ->
                case msg of
                    FormUpdated delta ->
                        let
                            ( state, cmd ) =
                                userForm.update delta model.state
                        in
                        ( { model | state = state }, cmd )

                    _ ->
                        ( model, Cmd.none )
        , view =
            \model ->
                { title = "Hello World"
                , body = Ui.singleton [ ( "Form", userForm.view model.state ) ]
                , layout = Layout.default
                }
        }
