module Markdown exposing (md, syntaxHighlighting)

import Html exposing (Html)
import Html.Attributes as Attr
import Less.Ui
import Less.Ui.Html exposing (Ui)
import Markdown.Block as Block
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer exposing (Renderer)
import Result.Extra as Result
import SyntaxHighlight


md : String -> Ui region narrowMsg msg
md =
    let
        heading :
            { level : Block.HeadingLevel, rawText : String, children : List (Ui region narrowMsg msg) }
            -> Ui region narrowMsg msg
        heading { level, children } =
            List.concat children
                |> (case level of
                        Block.H1 ->
                            Less.Ui.Html.block "h1" []

                        Block.H2 ->
                            Less.Ui.Html.block "h2" []

                        Block.H3 ->
                            Less.Ui.Html.block "h3" []

                        Block.H4 ->
                            Less.Ui.Html.block "h4" []

                        Block.H5 ->
                            Less.Ui.Html.block "h5" []

                        Block.H6 ->
                            Less.Ui.Html.block "h6" []
                   )

        paragraph : List (Ui region narrowMsg msg) -> Ui region narrowMsg msg
        paragraph =
            List.concat >> Less.Ui.Html.block "p" []

        blockQuote : List (Ui region narrowMsg msg) -> Ui region narrowMsg msg
        blockQuote =
            List.concat >> Less.Ui.Html.block "blockquote" []

        html : Markdown.Html.Renderer (List (Ui region narrowMsg msg) -> Ui region narrowMsg msg)
        html =
            Markdown.Html.oneOf []

        text : String -> Ui region narrowMsg msg
        text =
            Html.text >> List.singleton >> Less.Ui.singleton

        codeSpan : String -> Ui region narrowMsg msg
        codeSpan =
            text >> Less.Ui.Html.inline "code" []

        strong : List (Ui region narrowMsg msg) -> Ui region narrowMsg msg
        strong =
            List.concat >> Less.Ui.Html.inline "strong" []

        emphasis : List (Ui region narrowMsg msg) -> Ui region narrowMsg msg
        emphasis =
            List.concat >> Less.Ui.Html.inline "em" []

        strikethrough : List (Ui region narrowMsg msg) -> Ui region narrowMsg msg
        strikethrough =
            List.concat >> Less.Ui.Html.inline "span" [ Attr.style "text-decoration-line" "line-through" ]

        hardLineBreak : Ui region narrowMsg msg
        hardLineBreak =
            Less.Ui.singleton [ Html.br [] [] ]

        link : { title : Maybe String, destination : String } -> List (Ui region narrowMsg msg) -> Ui region narrowMsg msg
        link config content =
            case config.title of
                Just title ->
                    Less.Ui.Html.inline "a"
                        [ Attr.href config.destination
                        , Attr.title title
                        ]
                        (List.concat content)

                Nothing ->
                    Less.Ui.Html.inline "a" [ Attr.href config.destination ] (List.concat content)

        image : { alt : String, src : String, title : Maybe String } -> Ui region narrowMsg msg
        image imageInfo =
            case imageInfo.title of
                Just title ->
                    Less.Ui.Html.inline "img"
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        , Attr.title title
                        ]
                        []

                Nothing ->
                    Less.Ui.Html.inline "img"
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        ]
                        []

        unorderedList : List (Block.ListItem (Ui region narrowMsg msg)) -> Ui region narrowMsg msg
        unorderedList =
            List.concatMap
                (\item ->
                    case item of
                        Block.ListItem task children ->
                            let
                                checkbox =
                                    case task of
                                        Block.NoTask ->
                                            Less.Ui.singleton [ Html.text "" ]

                                        Block.IncompleteTask ->
                                            Less.Ui.Html.block "input"
                                                [ Attr.disabled True
                                                , Attr.checked False
                                                , Attr.type_ "checkbox"
                                                ]
                                                (Less.Ui.singleton [ Html.text "" ])

                                        Block.CompletedTask ->
                                            Less.Ui.Html.block "input"
                                                [ Attr.disabled True
                                                , Attr.checked True
                                                , Attr.type_ "checkbox"
                                                ]
                                                (Less.Ui.singleton [ Html.text "" ])
                            in
                            Less.Ui.Html.block "li" [] (checkbox ++ List.concat children)
                )
                >> Less.Ui.Html.block "ul"
                    []

        orderedList : Int -> List (List (Ui region narrowMsg msg)) -> Ui region narrowMsg msg
        orderedList startingIndex =
            List.concatMap
                (\itemBlocks ->
                    Less.Ui.Html.block "li"
                        []
                        (List.concat itemBlocks)
                )
                >> Less.Ui.Html.block "ol"
                    (case startingIndex of
                        1 ->
                            [ Attr.start startingIndex ]

                        _ ->
                            []
                    )

        codeBlock : { body : String, language : Maybe String } -> Ui region narrowMsg msg
        codeBlock block =
            if block.language == Just "elm" then
                SyntaxHighlight.elm block.body
                    |> Result.unpack
                        (\_ -> Html.text "Error in `code`")
                        (SyntaxHighlight.toBlockHtml Nothing)
                    |> List.singleton
                    |> Less.Ui.singleton
                    |> Less.Ui.Html.block "div" []

            else
                Less.Ui.Html.block "pre"
                    []
                    (Less.Ui.singleton
                        [ Html.code []
                            [ Html.text block.body
                            ]
                        ]
                    )

        thematicBreak : Ui region narrowMsg msg
        thematicBreak =
            Less.Ui.singleton [ Html.hr [] [] ]

        table : List (Ui region narrowMsg msg) -> Ui region narrowMsg msg
        table =
            List.concat >> Less.Ui.Html.block "table" []

        tableHeader : List (Ui region narrowMsg msg) -> Ui region narrowMsg msg
        tableHeader =
            List.concat >> Less.Ui.Html.block "thead" []

        tableBody : List (Ui region narrowMsg msg) -> Ui region narrowMsg msg
        tableBody =
            List.concat >> Less.Ui.Html.block "tbody" []

        tableRow : List (Ui region narrowMsg msg) -> Ui region narrowMsg msg
        tableRow =
            List.concat >> Less.Ui.Html.block "tr" []

        tableCell : Maybe Block.Alignment -> List (Ui region narrowMsg msg) -> Ui region narrowMsg msg
        tableCell =
            \_ -> List.concat >> Less.Ui.Html.block "td" []

        tableHeaderCell : Maybe Block.Alignment -> List (Ui region narrowMsg msg) -> Ui region narrowMsg msg
        tableHeaderCell maybeAlignment =
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            List.concat >> Less.Ui.Html.block "th" attrs

        elmRenderer : Renderer (Ui region narrowMsg msg)
        elmRenderer =
            { heading = heading
            , paragraph = paragraph
            , blockQuote = blockQuote
            , html = html
            , text = text
            , codeSpan = codeSpan
            , strong = strong
            , emphasis = emphasis
            , strikethrough = strikethrough
            , hardLineBreak = hardLineBreak
            , link = link
            , image = image
            , unorderedList = unorderedList
            , orderedList = orderedList
            , codeBlock = codeBlock
            , thematicBreak = thematicBreak
            , table = table
            , tableHeader = tableHeader
            , tableBody = tableBody
            , tableRow = tableRow
            , tableCell = tableCell
            , tableHeaderCell = tableHeaderCell
            }
    in
    Markdown.parse
        >> Result.mapError (List.map Markdown.deadEndToString >> String.join "\n")
        >> Result.andThen (Markdown.Renderer.render elmRenderer)
        >> Result.map List.concat
        >> Result.extract (Html.text >> List.singleton >> Less.Ui.singleton)


syntaxHighlighting : Html msg
syntaxHighlighting =
    SyntaxHighlight.useTheme SyntaxHighlight.gitHub
