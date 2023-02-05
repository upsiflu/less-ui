# Less power, less control? Less go for it!

- No Url in your Model: The complete Ui state is resides in the Url, not in your Elm code

(in other words: Get rid of user interface related `Msg`s
-> `Application`)

- No mingling of logic and presentation: Write a `view` for your type, then declare how the views are composed: 
  - `with : aspect -> Ui -> Ui -> Ui` to nest elements semantically
  - `wrap : wrapper -> Ui -> Ui` to nest elements of one semantic aspect visually
  - `(++) : Ui -> Ui -> Ui` to concatenate Uis. 
- No layout in your views: Define soft transitions, visual wrappers, and a global semantic Layout only once.

**Outside of the scope of this package:**

This package can help you create predictable and pleasurable interfaces without making design decisions. It is not a complete Ui package.

- No inaccessible, invalid, inconsistently styled Html – Use _elm-w3_, _elm-widgets_, _elm-ui_ and friends
- No interactive controls without corresponding Model types: This will probably be a separate package because it's beyond the scope of this one. I love
  the concept of Codecs _elm-codec_, _elm-url-codec_) where you define two-way relations between types. 
  The new package will be a Codec between your Model type and a corresponding Component, adhering to UX best practices.

**Comparison to fellow Ui and SPA frameworks:**

- It is headless, so you need to bring your own widget framework such as _elm-widgets_ and _elm-ui_.
  There is a convergence in the Ui APIs, and Restrictive adheres to this standard so that you can easily
  integrate it with these. No need to learn yet another API.
- TODO: On the side of Routing and Data it aims to work well with elm-pages v3.
- Being `Restrictive`, the focus is on offering a very limited set of semantically sound and conventional features to manage Ui state. 
  All interactive components this library offers are "Links" that map from a given Url to another. As far as possible, they should
  render as `<a href>`, and only Tabs should render as `<button>`s. You need to use other libraries to modify your `Model`.
- You get high degree of cohesion so it's useful for quick, small projects, unlike _elm-pages_ which allows for stronger decoupling (?).

**By integrating `Restrictive`, you get the following benefits:**

- The Url stores your complete Ui state. No more Cmd`s (like the modeless components in elm-widgets), plus reproducible Ui state.
- No more inter-component messaging! Define your module and type hierarchy according to the needs of your data, then define the
  views for each type. Use `with : place -> element -> element` to designate a place in a global layout for your component.
- Migrate to layouts and even frameworks without touching your ui code. Just supply a different `Layout`. In a layout, you can
  turn on soft transitions between all Ui states, respond to the screen dimensions, or modify visual containers.

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

You can verify the examples given in the docs by running

```shell
npm install -g elm-verify-examples


elm-verify-examples && elm-test && rm -r ./tests/VerifyExamples
```






## [Ui](Ui): Flat layout instead of nested components

First construct a semantic tree, tagging all your Ui elements with [semantic Aspect](Ui.Layout.Aspect)s. 

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


## [Application](Ui.Application): Sever Route from Model

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

## [Link](Ui.Link): Manage the Ui State as a URL

While the Url you see in your Address bar represents the current [Ui `State`](Ui.State) of your app,
a [`Link`](Ui.Link#Link) represents a relative change to it.

This is inspired by TEA: `State` is the Model, `Link` is a Msg, and `State.update` is the update function. 

  - [`GoTo`](Ui.Link#toggle) a path and/or fragment (which you handle in your `view`)
  - [`Toggle`](Ui.Link#toggle) some [Controls](Ui.Layout.Aspect), for example user preferences or a toolbar
  - [`Bounce`](Ui.Link#bounce) between expanded and collapsed states inside an interactive tree view, accordion, or nested diagram
