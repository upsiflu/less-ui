module Markdown exposing (html, md, syntaxHighlight, toc)

import Html exposing (Html)
import Html.Attributes as Attr
import Less.Ui as Ui
import Less.Ui.Html as Ui
import Markdown.Block as Block
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer exposing (Renderer)
import Result.Extra as Result
import SyntaxHighlight


heading :
    { level : Block.HeadingLevel, rawText : String, children : List (Ui.Html region narrowMsg msg) }
    -> Ui.Html region narrowMsg msg
heading { level, rawText, children } =
    let
        id =
            String.replace " " "+" rawText
    in
    List.concat children
        |> (case level of
                Block.H1 ->
                    Ui.block "h1" [ Attr.id id ]

                Block.H2 ->
                    Ui.block "h2" [ Attr.id id ]

                Block.H3 ->
                    Ui.block "h3" [ Attr.id id ]

                Block.H4 ->
                    Ui.block "h4" [ Attr.id id ]

                Block.H5 ->
                    Ui.block "h5" [ Attr.id id ]

                Block.H6 ->
                    Ui.block "h6" [ Attr.id id ]
           )


paragraph : List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg
paragraph =
    List.concat >> Ui.block "p" []


blockQuote : List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg
blockQuote =
    List.concat >> Ui.block "blockquote" []


html_ : Markdown.Html.Renderer (List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg)
html_ =
    Markdown.Html.oneOf []


text : String -> Ui.Html region narrowMsg msg
text =
    Html.text >> List.singleton >> Ui.singleton


codeSpan : String -> Ui.Html region narrowMsg msg
codeSpan str =
    if String.startsWith "#!elm" str then
        String.dropLeft 5 str
            |> SyntaxHighlight.elm
            |> Result.unpack
                (\_ -> Html.text "Error in `code`")
                SyntaxHighlight.toInlineHtml
            |> List.singleton
            |> Ui.singleton

    else
        text str
            |> Ui.inline "code" []


strong : List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg
strong =
    List.concat >> Ui.inline "strong" []


emphasis : List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg
emphasis =
    List.concat >> Ui.inline "em" []


strikethrough : List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg
strikethrough =
    List.concat >> Ui.inline "span" [ Attr.style "text-decoration-line" "line-through" ]


hardLineBreak : Ui.Html region narrowMsg msg
hardLineBreak =
    Ui.singleton [ Html.br [] [] ]


link : { title : Maybe String, destination : String } -> List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg
link config content =
    case config.title of
        Just title ->
            Ui.inline "a"
                [ Attr.href config.destination
                , Attr.title title
                ]
                (List.concat content)

        Nothing ->
            Ui.inline "a" [ Attr.href config.destination ] (List.concat content)


image : { alt : String, src : String, title : Maybe String } -> Ui.Html region narrowMsg msg
image imageInfo =
    case imageInfo.title of
        Just title ->
            Ui.inline "img"
                [ Attr.src imageInfo.src
                , Attr.alt imageInfo.alt
                , Attr.title title
                ]
                []

        Nothing ->
            Ui.inline "img"
                [ Attr.src imageInfo.src
                , Attr.alt imageInfo.alt
                ]
                []


unorderedList : List (Block.ListItem (Ui.Html region narrowMsg msg)) -> Ui.Html region narrowMsg msg
unorderedList =
    List.concatMap
        (\item ->
            case item of
                Block.ListItem task children ->
                    let
                        checkbox =
                            case task of
                                Block.NoTask ->
                                    Ui.singleton [ Html.text "" ]

                                Block.IncompleteTask ->
                                    Ui.block "input"
                                        [ Attr.disabled True
                                        , Attr.checked False
                                        , Attr.type_ "checkbox"
                                        ]
                                        (Ui.singleton [ Html.text "" ])

                                Block.CompletedTask ->
                                    Ui.block "input"
                                        [ Attr.disabled True
                                        , Attr.checked True
                                        , Attr.type_ "checkbox"
                                        ]
                                        (Ui.singleton [ Html.text "" ])
                    in
                    Ui.block "li" [] (checkbox ++ List.concat children)
        )
        >> Ui.block "ul"
            []


orderedList : Int -> List (List (Ui.Html region narrowMsg msg)) -> Ui.Html region narrowMsg msg
orderedList startingIndex =
    List.concatMap
        (\itemBlocks ->
            Ui.block "li"
                []
                (List.concat itemBlocks)
        )
        >> Ui.block "ol"
            (case startingIndex of
                1 ->
                    [ Attr.start startingIndex ]

                _ ->
                    []
            )


codeBlock : { body : String, language : Maybe String } -> Ui.Html region narrowMsg msg
codeBlock block =
    if block.language == Just "elm" then
        SyntaxHighlight.elm block.body
            |> Result.unpack
                (\_ -> Html.text "Error in `code`")
                (SyntaxHighlight.toBlockHtml Nothing)
            |> List.singleton
            |> Ui.singleton
            |> Ui.block "div" []

    else
        Ui.block "pre"
            []
            (Ui.singleton
                [ Html.code []
                    [ Html.text block.body
                    ]
                ]
            )


thematicBreak : Ui.Html region narrowMsg msg
thematicBreak =
    Ui.block "hr" [] (text "")


table : List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg
table =
    List.concat >> Ui.block "table" []


tableHeader : List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg
tableHeader =
    List.concat >> Ui.block "thead" []


tableBody : List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg
tableBody =
    List.concat >> Ui.block "tbody" []


tableRow : List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg
tableRow =
    List.concat >> Ui.block "tr" []


tableCell : Maybe Block.Alignment -> List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg
tableCell =
    \_ -> List.concat >> Ui.block "td" []


tableHeaderCell : Maybe Block.Alignment -> List (Ui.Html region narrowMsg msg) -> Ui.Html region narrowMsg msg
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
    List.concat >> Ui.block "th" attrs


elmRenderer : Renderer (Ui.Html region narrowMsg msg)
elmRenderer =
    { heading = heading
    , paragraph = paragraph
    , blockQuote = blockQuote
    , html = html_
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


md : String -> Ui.Html region narrowMsg msg
md =
    Markdown.parse
        >> Result.mapError (List.map Markdown.deadEndToString >> String.join "\n")
        >> Result.andThen (Markdown.Renderer.render elmRenderer)
        >> Result.map List.concat
        >> Result.extract text


html : String -> List (Html msg)
html =
    Markdown.parse
        >> Result.mapError (List.map Markdown.deadEndToString >> String.join "\n")
        >> Result.andThen (Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer)
        >> Result.extract (Html.text >> List.singleton)


toc :
    String
    ->
        Result
            (Ui.Html region narrowMsg msg)
            (List
                { level : Block.HeadingLevel
                , raw : List Block.Inline
                , text : String
                }
            )
toc =
    Markdown.parse
        >> Result.mapError (List.map Markdown.deadEndToString >> String.join "\n" >> text)
        >> Result.map
            (List.filterMap
                (\block ->
                    case block of
                        Block.Heading level content ->
                            Just
                                { level = level
                                , raw = content
                                , text = Block.extractInlineText content
                                }

                        _ ->
                            Nothing
                )
            )


{-| Adds github syntax highlighting from <https://pablohirafuji.github.io/elm-syntax-highlight/>
-}
syntaxHighlight : Ui.Html region narrowMsg msg
syntaxHighlight =
    Ui.html [ SyntaxHighlight.useTheme SyntaxHighlight.gitHub ]
