# Build restricted interfaces

### Try the examples:
```shell
cd examples

npm install -g elm-watch
npm ci

npm start
```

Go to [localhost:8001](http://localhost:8001/) and open one of the examples.

[Read more...](examples/README.md)

### Read the documentation:
```shell
npm install -g elm-doc-preview

edp
```

Now check out [localhost:8000](http://localhost:8000/)

# Less power, less control, less fun? --- Less go for it!


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


## [Application](Ui.Application): Separate Route from Model

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

## [Link](Ui.Link): Manage [the Ui State](Ui.State) as a URL

While the Url you see in your Address bar represents the current [Ui `State`](Ui.State) of your app,
a [`Link`](Ui.Link#Link) represents a relative change to it.

This is inspired by TEA: `State` is the Model, `Link` is a Msg, and `State.update` is the update function. 


  - [`Toggle`](Ui.Link#toggle) some [Controls](Ui.Layout.Aspect), for example user preferences or a toolbar
  - [`Bounce`](Ui.Link#bounce) between expanded and collapsed states inside an interactive tree view, accordion, or nested diagram
