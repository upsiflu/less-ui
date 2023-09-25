# Less power, less control? Less go for it!

An proof-of-concept package for people who don't want to hand-wrangle user interface logic. Don't use it when you need direct control over the user interface. 

🐌 I'm curious what you think! Write me on `upsiflu@gmail.com` 🐌


## Goals

**1. Cohesion over flexibility:** The `view` in each module of a SPA should be as self-contained as possible. This benefits small, quick projects the most.

**2. Write less code:** Provide presets for the most common layout and state-related Ux patterns.


## Non-Features

- No direct control over every pixel. Use elm-ui instead if you want a crafted design.
- No intercepting the Url except for `Filter` which gives you the current query flags. Use Browser.application and friends to roll your own Url decoder.
- No constraints on your Html. Use _elm-w3_ if correctness is important.
- This library has been used in three small SPA projects and is very much in flux.

## Features

**Let the Url store all the Ui state** - No more Ui messages in your application. Bonus: you can reproduce the current Ui state by copying the Url. Bonus 2: You can style state transitions for extra smoothness.

**Target several screen regions in a single view** so you don't need to aggregate Html snippets in some unrelated module.

```elm
    at Scene (textLabel "Scene") ++ at Info (textLabel "Info")
```

```
    ┏━━━━━━━━━━━┓
    ┃  Header   ┃ 👈 Tabs, toolbars, menus...
    ┠───────────┨
    ┃  Scene    ┃ 👈 The composition your user reads or edits
    ┃      ╭────┨
    ┃      │Info┃ 👈 Toasts, popups...
    ┠──────┴────┨
    ┃  Control  ┃ 👈 Editing tools and progressive disclosure
    ┗━━━━━━━━━━━┛
```

**Compose everything:** `at region`, `(++)`, `wrap wrapper` compose Uis with each other. Uis can be created from anything that renders to Html (`elm-ui`, `elm-widgets`, `String`...) or from a limited set of patterns including `toggle`, `search`, `filter`, `goTo` etc. You can also use widgets that compose Html such as `elm-any-type-form`.




## Try it
```shell
npm install -g elm-watch

cd examples
npm ci
npm start
```

Go to [localhost:8001](http://localhost:8001/) and open one of the examples.

Read [the library README.md (this doc)](http://features.localhost:8099/packages/upsiflu/less-ui/latest).

Read [examples/README.md](http://localhost:8098).


## Docs
```shell
npm install -g elm-doc-preview

edp
```

Now check out [localhost:8000](http://localhost:8000/)

**Verify the doc snippets:**

```shell
npm install -g elm-verify-examples

elm-verify-examples && elm-test && rm -r ./tests/VerifyExamples
```

**Review the code:**

```shell
npm install -g elm-review

elm-review
```

**Outside of the scope of this package:**

This package can help you create predictable and pleasurable interfaces without making design decisions. It is not a complete Ui package.

- No inaccessible, invalid, inconsistently styled Html – Use _elm-w3_, _elm-widgets_, _elm-ui_ and friends
- No interactive controls without corresponding Model type – Use _elm-any-type-forms_



## Use case

In small apps, `less-ui` can reduce the `view` code. In the following example, `Html.toggle` adds state without cluttering the model, and `Ui.at` spreads a single view over separate screen regions (branches of the DOM).

  ```elm
  import Ui
  import Ui.Html as Html
  import Ui.Region exposing (Region(..))



  makeTab (name, src, description) =
      let
          photo =
              Ui.singleton [Html.img [Attr.src photo] []]

          caption =
              Ui.singleton [Html.text description]
        
      in
      Ui.at Scene photo ++ Ui.at Info caption
          |> Html.toggle []
              { flag = name
              , isGlobal = True
              , label = Html.text name 
              }

  view =
      Ui.singleton [Html.text "Look at these trees:"]
          ++ List.concatMap makeTab
            [ ("Elm", "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d2/East_Coker_elm%2C_2.jpg/440px-East_Coker_elm%2C_2.jpg", "Its planky wood makes the Elm tree a hikers' favorite.")
            , ("Yggdrasill", "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b3/Om_Yggdrasil_by_Fr%C3%B8lich.jpg/440px-Om_Yggdrasil_by_Fr%C3%B8lich.jpg", "You cannot sleep here but you may find fruit and feathers.")
            , ("Trie","https://upload.wikimedia.org/wikipedia/commons/thumb/b/be/Trie_example.svg/500px-Trie_example.svg.png", "The Trie is a noble pine wihtout wheels.")
            ]

  
  ```