module Garden.Rhododendron exposing (Rhododendron, singleton, update, Msg, view)

{-|

@docs Rhododendron, singleton, update, Msg, view

-}

import Html exposing (Html, button, input, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Ui exposing (Ui)
import Ui.Layout.Aspect exposing (Aspect(..))


{-| -}
type Rhododendron
    = Rhododendron String (List Rhododendron)


{-| -}
singleton : String -> Rhododendron
singleton s =
    Rhododendron s []


type alias Path =
    String


{-| -}
type Msg
    = Edit Path String
    | Grow


{-| -}
update : Msg -> Rhododendron -> Rhododendron
update msg ((Rhododendron label descendants) as rhodo) =
    case msg of
        Edit _ newLabel ->
            Rhododendron newLabel descendants

        Grow ->
            Rhododendron label (singleton (String.fromInt (count rhodo)) :: descendants)


{-| -}
growAt : Path -> Rhododendron -> Rhododendron
growAt path =
    map
        (\(Rhododendron label descendants) -> label)


getPath : Rhododendron -> Path
getPath (Rhododendron label descendants) =
    String.join ", " (List.map getPath descendants) ++ "🌺"


fromRoot : (List Rhododendron -> Rhododendron) -> Rhododendron -> Rhododendron
fromRoot fu (Rhododendron label descendants) =
    Rhododendron label <|
        case descendants of
            [] ->
                [ fu [] ]

            list ->
                List.map (fromRoot fu) list


count : Rhododendron -> Int
count (Rhododendron _ descendants) =
    List.sum (List.map count descendants) + 1


{-| -}
map : (Rhododendron -> Path) -> Rhododendron -> Rhododendron
map toPath ((Rhododendron label descendants) as rhodo) =
    Rhododendron (toPath rhodo) (List.map (map toPath) descendants)


{-| -}
view : (Msg -> msg) -> Rhododendron -> Ui (Html msg)
view howToMessage ((Rhododendron label _) as rhododendron) =
    let
        controls : Ui (Html msg)
        controls =
            button [ onClick (Grow |> howToMessage) ] [ text "append" ]
                |> Ui.html
                |> (++) (input [ onInput (Edit label >> howToMessage) ] [ Html.text label ] |> Ui.html |> Ui.addTextLabel "Edit")

        scene : Rhododendron -> Ui (Html msg)
        scene (Rhododendron s descendants) =
            Ui.singleton
                |> Ui.with Scene (Ui.textLabel s)
                |> Ui.with Scene (List.concatMap scene descendants)
                |> Ui.wrap (Html.Keyed.ul [ Attr.class "rhododendron" ] >> Tuple.pair s >> List.singleton)
    in
    scene rhododendron
        |> Ui.with Control controls
