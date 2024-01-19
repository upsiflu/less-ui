# Less power, less control? Less go for it

**A proof-of-concept package for people who don't want to hand-wrangle user interface logic.**

**[Live demo](https://less-ui.web.app/)**

[Start the demo on your computer](#examples)

 _I'm curious what you think! Write me on `upsiflu@gmail.com` or add an [issue on github](https://github.com/upsiflu/less-ui/issues)_

### 🐌 Goals

**1. Cohesion over flexibility:**

> The `view` in each module of a SPA should be as self-contained as possible. This benefits small, quick projects the most.

**2. Write less code:**

> Provide presets for the most common layout and state-related Ux patterns.

**3. Mix-and-match with other Ui libraries _(still work in progress)_:**

> The Api follows established conventions and offers clear boundaries for simple integration within frameworks such as elm-pages and elm-land, with helper libraries such as elm-widgets, and with type-centric libraries such as _elm-ui_ and _elm-multitool_. _elm-any-type-forms_ is a great fit as it has a similar goal: while less-ui maps interaction and layout patterns into Url state and links, elm-any-type-forms maps your application model into views with state and delta.

### ~~🐌~~ Non-Features

- No direct control over every pixel. Use _elm-ui_ if you are a designer.
- No default Ui widgets. Use _elm-widgets_ or the like.
- No intercepting the Url (except for `Filter`, a pattern that gives you the current query flags). Use Browser.application and friends to roll your own Url decoder.
- No constraints on your Html. Use _elm-w3_ if you want compile-time invariants for correctness and accessibility. Note that as of v2.0, _less-ui_ is not yet compatible with _elm-w3_.
- This library has been used in about three small SPA projects. It's not stable yet.

### 🐌 Features

**— Let the Url store all the Ui state —** No more Ui messages in your application.

- Use straightforward patterns such as `search`, `goto` or `toggle` to build interactivity.
- You can reproduce the current Ui state by copying the Url.
- Style state transitions with css for extra smoothness.

**— Target several screen regions in a single view —** so you don't need to push around Html snippets across your modules.

```elm
Ui.inRegion Scene (text "Scene") ++ Ui.inRegion Info (text "Info") ...
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

**— Compose everything:**

- Create Ui snippets from anything that you can map to `Html` (_elm-html_, _elm-markdown_, _elm-ui_, _elm-widgets_, _String_...) or use the default `Less.Ui.Html` module.
- Each snippet is a List so you can compose two with `++`.
- You can also use widgets that compose nested Html snippets such as `elm-any-type-form`.

## Docs

```shell
npm install -g elm-doc-preview


edp
```

Now check out [localhost:8000](http://localhost:8000/)

**Verify the mini-examples in the comments:**

```shell
npm install -g elm-verify-examples

elm-verify-examples && elm-test && rm -r ./tests/VerifyExamples
```

**Review the code:**

```shell
npm install -g elm-review

elm-review
```

## Examples

Includes [Simon Lydell's amazing elm-watch](https://github.com/lydell/elm-watch) for live coding; derived from [this example](https://github.com/lydell/elm-watch/tree/main/example).

**Start the live server:**

```shell
cd examples

npm install -g elm-doc-preview
npm ci
npm start
```

Go to [localhost:8001](http://localhost:8001/) and open one of the examples.

Read [the library README.md (this doc)](http://features.localhost:8099/packages/upsiflu/less-ui/latest).

1. Edit the files in src/ while watching a browser tab pointing at [localhost:8001](localhost:8001).
_<small>A note to vscodium users: To activate the language server in your editor, choose "Open Folder..." and select `examples`.</small>_
1. In that browser tab, you can activate the **Elm debugger** by clicking the `elm-watch` menu in the bottom left corner.
1. Click on the `elm-watch hot` buttons in your terminal output to check for errors.

### Demo

A list of features, with code and examples.
[[Features.elm]](../src/Features.elm)

Live server: [features.localhost:8001](http://features.localhost:8001)

latest demo online: <https://less-ui.web.app>

## Contribute

I'm always happy to see issues and code contributions from you. Make sure you have a global gitignore to keep editor and OS specific configs out of the loop. All dev prerequisites are listed in examples/package.json and can be installed with `cd examples && npm ci`. Node v11 is required for run-pty.

<p align="center" style="font-weight:bold;">
    Have a lot of fun <3</p>
<p align="center" style="font-size:1.5em;">🐌🐌🐌</p>
