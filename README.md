# Less power, less control? Less go for it!

This is an experimental package for people who don't want to hand-wrangle user interface logic. It's headless, meaning you bring your own components (elm-widget, elm-w3, elm-ui etc). Don't use it when you need direct control over the user interface.

**Let the Url store all the Ui state** - No more Ui messages in your application. Bonus: you can reproduce the current Ui state by copying the Url.

**Explicitly model state transitions as a tree** - `a |> with b` displays `b` only when `a` is active (_Progressive disclosure_). 

**Let your views appear in many places at once** - `a |> at Scene` moves `a` into the `Scene` `Region` of the screen without affecting its state transition relations:

  ```elm
  
  trees = 
      [ ("Elm", "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d2/East_Coker_elm%2C_2.jpg/440px-East_Coker_elm%2C_2.jpg", "Its planky wood makes the Elm tree a hikers' favorite.")
      , ("Yggdrasill", "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b3/Om_Yggdrasil_by_Fr%C3%B8lich.jpg/440px-Om_Yggdrasil_by_Fr%C3%B8lich.jpg", "You cannot sleep here but you may find fruit and feathers.")
      , ("Trie","https://upload.wikimedia.org/wikipedia/commons/thumb/b/be/Trie_example.svg/500px-Trie_example.svg.png", "The Trie is a noble pine wihtout wheels.")
      ]

  makeTab (name, src, description) =
      let
          photo =
              Ui.singleton (Html.img [Attr.src photo] [])

          caption =
              Ui.singleton (Html.text description)
        
      in
      Ui.toggle []
          { flag = name
          , isGlobal = True
          , label = Html.text name 
          }
              |> Ui.with ( Ui.at Scene photo ++ Ui.at Info caption )

  app =
      Ui.singleton (Html.text "Look at these trees:")
          ++ List.concatMap makeTab trees

  
  ```

**Try the examples:**
```shell
cd examples

npm install -g elm-watch
npm ci

npm start
```

Go to [localhost:8001](http://localhost:8001/) and open one of the examples.
Read [examples/README.md](examples/README.md).

**Perouse the docs:**
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


**Outside of the scope of this package:**

This package can help you create predictable and pleasurable interfaces without making design decisions. It is not a complete Ui package.

- No inaccessible, invalid, inconsistently styled Html – Use _elm-w3_, _elm-widgets_, _elm-ui_ and friends
- No interactive controls without corresponding Model types: This will probably be a separate package because it's beyond the scope of this one. I love
  the concept of Codecs _elm-codec_, _elm-url-codec_) where you define two-way relations between types. 
  The new package will be a Codec between your Model type and a corresponding Component, adhering to UX best practices.

**Comparison to fellow Ui and SPA frameworks:**

- It is headless, so you need to bring your own widget framework such as _elm-widgets_ and _elm-ui_.
- Being `Restrictive`, the focus is on offering a very limited set of semantically sound and conventional features to manage Ui state. 
  All interactive components this library offers are "Links" that map from a given Url to another. As far as possible, they should
  render as `<a href>`. You need to use other libraries to modify your `Model`.
- You get high degree of cohesion so it's useful for quick, small projects.



## [Ui](Ui): Flat layout instead of nested components

First construct a semantic tree, tagging all your Ui elements with [semantic Region](Restrictive.Layout.Region)s. 

```elm
    Ui.constant [Html.text "Handle"]
      |> Ui.with Scene (Ui.textLabel "Scene")
      |> Ui.with Info (Ui.textLabel "Info")
      |> Ui.with Control (Ui.textLabel "Control")
```

The [Layout](Ui.Layout#view) will flatten each aspect and render it in its place:

```
    ┏━━━━━━━━━━━┓
    ┃  Handle   ┃ 👈 Tabs, toolbars, menus...
    ┠───────────┨
    ┃  Scene    ┃ 👈 The composition your user reads or edits
    ┃      ╭────┨
    ┃      │Info┃ 👈 Toasts, popups...
    ┠──────┴────┨
    ┃  Control  ┃ 👈 Editing tools and progressive disclosure
    ┗━━━━━━━━━━━┛
```


## [Restrictive.Application](Restrictive#application): Sever Route from Model

Define your `Model` and `update` indepenent from the `Url` query.
The [Application](Ui.Application) catches changes to the `Url` and provides a simplified `application` function:

```elm
   application :
      { init : ( model, Cmd modelMsg )
      , update : modelMsg -> model -> ( model, Cmd modelMsg )
      , view : ( Path, Fragment ) -> model -> 
               { body : Ui modelMsg, layout : Layout, title : String }
      }
      -> Application model modelMsg
```

## [State.Link](Restrictive.State#Link): Manage the Ui State as a URL

While the Url you see in your Address bar represents the current [Ui `State`](Ui.State) of your app,
a [`Link`](Ui.Link#Link) represents a relative change to it.

This is inspired by TEA: `State` is the Model, `Link` is a Msg, and `State.update` is the update function. 

  - [`goTo`](Ui#toggle) a path and/or fragment (which you handle in your `view`)
  - [`toggle`](Ui#toggle) some [Controls](Restrictive.Layout.Region), for example user preferences or a toolbar
  - [`bounce`](Ui#bounce) between expanded and collapsed states inside an interactive tree view, accordion, or nested diagram
