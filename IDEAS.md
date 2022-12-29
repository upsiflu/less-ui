# Ideas for v1.0.0

### Patterns in version one:

- [x] **Flat Layout**.
  - `with` _`Aspect`_: compose `Ui`s into regions.
- [x] [**Progressive disclosure**. Mask interface elements hierarchically](README.md#relations-across-the-tree)
  - `toggle`: create a `Ui` that can be disclosed.
- [x] [**Sharable View**. Separate `Route` (source of truth: URL) from `Model`/`update` (source of truth: Elm application)](README.md#separate-the-route-from-your-model)
  - [`Application`](src/Application.elm):
```elm
    { init : ( model, Cmd modelMsg )
    , update : modelMsg -> model -> ( model, Cmd modelMsg )
    , view : Path -> model -> { body : Ui modelMsg, layout : Maybe Layout, title : String }
    }
```
- [ ] **Relative links**. A link may act differently, depending on the previous Route.
  - `bounce` _`Path p`_ _`Path q`_: Links to _p_, unless you are already there, in which case it links to _q_.
  - on LinkClicked, the incoming urlRequest is interpreted as relative: an incoming urlRequest `abc` on top of `xyz?f&q=r` will yield `abc?f&q=r`. This will enable manual relative links in precomposed Html.

### Patterns to implement later:
There is one open question which I am postponing to v1.1.0. I feel it requires some groundwork now to keep the concerns orthogonal.

- [ ] **Responsive Layout**. Some things just can't be done with CSS - because CSS has no explicit state.
  - Tabbing the `Control Sheets` of the current objects and exposing the current tab as a query flag.

Plans for versions beyond 1.1:

- [ ] **Restrictive Controls**. What if the `Model` type implies a comprehensive Widget? Instead of manually designing a `Ui` and wiring it up with the Model, introducing potential omissions or undesired states, an algorithm can derive a comprehensive Control Sheet for each of your Model constructors. Caveat is that for each constructor, the previously chosen parameters need to be kept in-memory (which is traditionally done by the Html DOM). We need to encode them either in the Url or in some local storage. Of course, in user-facing situations, you will want to handcraft a design instead, so it should be very easy to opt in and out locally.

- [ ] **Search Pattern**. How would a restrictive Api enable a search Api? Would it be a filter Api? Would it pertain to the window or separately to certain Scenes? What about autocompete tokens in a Control?

# Tabs

A tab-panel is a Ui datum that lives in the Url.
One tab per tablist is open.

A special case is a collapsible tablist, which can be represented by
`tab`s inside a `toggle`.

Since multiple assignments (`...?var=X&var=Y`) are valid Url queries,
we cannot enforce uniqueness by storing `tabPanelP=tabQ`.

Thus, simply storing the active tab per each tablist should be fine. 
This means the Ui must enforce unique names for each potential tab.

Q: Can we build a tablist just out of `toggle`s?

A: No. `(toggle "tab1" |> with Scene tab1)++(toggle "tab2" |> with Scene tab2)`
will create a global togglebar where multiple (or zero) tabs can be activated at once.

**Problems:**

1. There is no way to enforce uniqueness because concatenated `Ui`s are by definition
orthogonal to each other.

2. All tab-buttons will appear in the toggle-bar

## Target use cases

a. Google Docs mobile.

**Observation**

- A global bar offers the choice of zero or one options out of `[Style, Insert, Comment, Menu]`.
- The `Menu` expands from the right while the other options spawn a control sheet.
- The `Comment` option will change the viewmode, i.e. how the scene is presented.
- When `Style` is activated, all current items (insert-cursor or selection as well as the paragraph under the cursor) expand their control sheets.
- Parallel control sheets are presented as tabs, where one is open. Each control sheet generates a clickable tab-header that remains visible.

**Patterns**

- The global bar, among other functions, contains **proxies for the editing modes**
    - Editing modes are exclusive, and there is an unlabeled 'default' mode that is active when no other is.
    - An editing mode may change the viewmode of the scene, and may spawn a control sheet.
- A more traditional tab-bar appears in the `Control` region to switch between the control sheets for the different "current objects".

**Analysis**

The second pattern, the tab-bar, is a view strategy for space-constrained collapsing of control sheets. It requires unique names among the control sheets of objects that can be "current" ("under the cursor") at the same time.

The first one is more complicated:

```elm
List.concat
    [ toggle "Style" 'A' |> with Control styleControls |> with Scene normalScene
    , toggle "Insert" '+' |> with Control insertControls |> with Scene normalScene
    , toggle "Comment" 'M' |> with Scene commentsScene
    , toggle "Menu" '⋮' |> with Control mainMenu
    ]
```

The above implementation cannot enforce uniqueness.

We have two options now for enforcing uniqueness:

(a) implementing a function `radio` that allows exactly one choice among a list. This would break the current `Item` model where one `Item` has one `Handle`, one `Scene`, etc: Would the radio be a single `Handle`, with states beyond on and off? How would it interfere with the `Get` in the Item? Would it break the convention that Aspects in an Item are completely orthogonal? Or would the Item itself be a multitude of parallel implementations?

```elm
Ui.either
    [ ("Style", 'A', with Control styleControls >> with Scene normalScene)
    , ("Insert", '+', with Control insertControls >> with Scene normalScene)
    , ("Comment", 'M', with Scene commentsScene)
    , ("Menu", '⋮', with Control mainMenu)
    ]
```


(b) implement a `Descendent` constructor `Or` that parallelizes several Items:

```elm
Ui.either
    [ Ui.toggle "Style" 'A' |> with Control styleControls |> with Scene normalScene
    , Ui.toggle "Insert" '+' |> with Control insertControls |> with Scene normalScene
    , Ui.toggle "Comment" 'M' |> with Scene commentsScene
    , Ui.toggle "Menu" '⋮' |> with Control mainMenu
    ]
```

Problem: The Api dosn't allow for constructing Items, only lists of Descendants.
What if you wrap `either` around some Ui without handle? In fact, the viewmodel has items without handle. We can simply define that these fall outside the scope of either, but that would introduce weirdness where
    `a ++ Ui.either [b, c]` would equal `Ui.either [a, b, c]`.
Alternatively, we can define that the implicit handle to foliage is its index inside the list.


(c) Make it super-builder and add a binary constructor

```elm
Ui.toggle "Style" 'A' |> with Control styleControls |> with Scene normalScene
    |> Ui.or ( Ui.toggle "Insert" '+' |> with Control insertControls |> with Scene normalScene ) 
    |> Ui.or ( Ui.toggle "Comment" 'M' |> with Scene commentsScene )
    |> Ui.or ( Ui.toggle "Menu" '⋮' |> with Control mainMenu )
```






## Tabs in restrictive-Ui

In the Google Docs example, we have a tree-shaped data structure where each level may have a different control sheet attached to the object under the cursor. The tree shape may be explicit or implicit. For example, when we activate the `style` edit-mode, we get tabs for

- the insert-cursor _or_ the text-selection
- the current paragraph(s)
- _or_ the currently selected table-cell.

We don't want to explicitly define the view-logic but the cursor-logic, and the view-logic arises from it.

That control sheets are treated as exclusive tabs instead of a single concatenation, and can be designated by an 'arrangement flag' inside the logic (`Get`) tree. It is very similar to `Wrap`, which adds a 'rendering directive' into the `Get` tree. So let's call it `Tab`. The question is, how to extract tab-headers and tab-uids?

Currently, the constructor `Leaf` is defined as `Foliage -> Maybe Item -> Descendant`.
Each Item has a Handle, and a Handle may provide both tab-header and tab-uuid.
So we can reformulate `Leaf : Foliage -> List Item -> Descendant` where the items are exclusive to each other :-)

How are Leaves generated?
(1) ZERO ITEMS: by Html, which generates empty item-lists
(2) ONE ITEM: `with : Aspect -> Ui -> Ui -> Ui`: each descendant (or wrapped) `Leaf`
    - becomes a singleton `Get` item if no item was present, with an auto-generated `Constant` item
    - otherwise gets the sub-Ui appended into the single item's `Get`.
(3) ONE ITEM: with `toggle`: creates just a single Item and no foliage.

Now how do we build exclusive items?

(a) implement a function `merge : Aspect -> Ui -> Ui` that compresses several leaves into one, and then defining a default view for more-than-one items
(b) implement a builder-function `either` that generates a `Leaf` with several items
(c) implement a builder-function `orWith` that `cons`es to the list of items
(d) simply define that multiple `Control` Items are always tabbed

Both (b) and (c) have serious issues: `either` is inconvenient because it requires the builder to follow the `view` instead of the logical structure of the app type. (a) makes tabs explicit, which is against the idea of the library that Ui patterns should be implicit. (d) is nice if we can produce a great algorithm, for example respecting the viewport width. This means, we hook a global, declarative view-rule into the `Aspect` system such as

```elm
tabControlItems : ViewRule
tabControlItems =
    { hook = 
    }
```

Problem: Items in a Ui are no longer orthogonal. Or are they? Perhaps tabbing is exactly this!

Problem 2: Are we in _build_, _view_ or _layout_? 

We can formulate the tenet that build operations never touch deep, but that may not be useful...



**CREATE**
 - singleton
 - html
 - toggle, bounce...

**MODIFY**
 - wrap, map...

**COMPOSE**
 - with, ++

**VIEW**
```elm
viewUi =

    ViewModel.concatMap <|

        | Twig -> Item?
                    item.get
                        |> for each key, viewUi ( key, mask >> itemMask )
                        |> values [ Scene, Control, Info ]
                        |> concat
                        |> append handle: itemHandle

                  Otherwise empty ViewModel

               -> then append twig-foliage into viewModel per current aspect


        | Wrap -> view ui

               -> then wrap per current aspect
```



**LAYOUT**