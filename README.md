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

## Less power, less control, less fun? --- Less go for it!


### (1) Relations across the tree

Since the Dom has the shape of a tree, most Ui libraries let you construct hierarchical structures, nesting widges inside each other. Instead of mixing semantic and presentational concerns in a single tree, the _Restrictive_ Ui lets you first construct a semantic tree, allowing you to define four [Aspect]s(Ui.Layout.Aspect)s in each node. For example, you want to show `Info`rmation at a certain place on the screen whenever some object in your `Scene` is selected, or you want to provide `Control`s to edit nested objects in a composition when they are selected, or you want to show a `Handle` for the user to reveal advanced `Control`s. Place them inside the selected objects, and then let [Layout `view`](Ui.Layout#view) render these aspects into separate regions, ignoring their position in the tree but preserving their logical relation.

```
    ┏━━━━━━━━━━━┓
    ┃  Handle   ┃ 👈 Toggle the visibility of different pages or tools
    ┠───────────┨
    ┃  Scene    ┃ 👈 The composition your user reads or edits
    ┃      ╭────┨
    ┃      │Info┃ 👈 Toasts, popups...
    ┠──────┴────┨
    ┃  Control  ┃ 👈 for editing or navigating the Scene
    ┗━━━━━━━━━━━┛
```

![Top row: `Handle`s (to toggle global modes); center: `Scene` (your Model); speech bubbles: `Info`; bottom rows: `Control`](assets/modes3.svg)

[Case study: Reproducing the Google Docs mobile Ui](cases/Case.md)

### (2) Separate the Route from your Model

**Why?** In the conventional Elm architecture, if you want the `view` to respect state encoded in the `Url` such as queries or flags, you need your `update` to incorporate this state into the in-memory application Model, which is eventually interpreted in the `view`:
 
     - - Url ↘               Cmd ⭢ Url' - - -
             Message ↘      ↗ 
                      update       
     - - - - - Model ↗      ↘ Model' - - - - -
                                  ↘ 
                                  view

This opens two possible pitfalls:

- The Url can potentially manipulate parts of the Model that are not only intended for viewing, for example data shared with the backend
- The Model needs to parse the `Url` and pass it to the `view`, potentially losing or misinterpreting information.

**How?**

    - - - Url ⭢ (Route) ⭢ Url' - - - -
                               ↘
                               view  
                               ↗
    - - - - -  Model ↘    ↗ Model' - - -
                     update       
             Message ↗    ↘ Cmd



- Your `update` doesn't know about the `Url`
- Your `view` sees the `Url`, but you may not need to parse it because `Ui` provides some common patterns to manipulate the Route interactively:
  - [toggle](Ui#toggle) some [Controls](Ui.Layout.Aspect), for example user preferences or a toolbar
  - [alternate](Ui#alternate) between expanded and collapsed states inside an interactive tree view, accordion, or nested diagram.

The `Ui` module provides a simplified `application` type:

```elm
   application :
        { init : ( model, Cmd modelMsg )
        , update : modelMsg -> model -> ( model, Cmd (Msg modelMsg) )
        , view : ( ( Url, Key ), model ) -> Document modelMsg
        }
        -> Application model modelMsg

    type alias Application model modelMsg =
        Program () ( ( Url, Key ), model ) (Msg modelMsg)
```
