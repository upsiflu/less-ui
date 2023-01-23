# Examples

Includes [Simon Lydell's amazing elm-watch](https://github.com/lydell/elm-watch) for live coding; derived from [this example](https://github.com/lydell/elm-watch/tree/main/example).


**Install the live server:**

```shell
npm install -g elm-watch
npm ci
```

**Toy around:**
```shell
npm install -g elm-doc-preview
npm start
```

1. Edit the files in src/ while watching a browser tab pointing at [localhost:8001](localhost:8001). 
_<small>A note to vscodium users: To activate the language server in your editor, choose "Open Folder..." and select `examples`.</small>_
1. In that browser tab, you can activate the **Elm debugger** by clicking the `elm-watch` menu in the bottom left corner.
1. Browse the **documentation** at [localhost:8000](http://localhost:8000/packages/upsiflu/restrictive-examples/latest).
1. Click on the `elm-watch hot` buttons in your terminal output to check for errors.

**Have a lot of fun <3**

<p align="center" style="font-size:1.5em;">🐌🐌🐌</p>

## Features

A list of features, with code and examples. 
[[Features.elm]](../src/Features.elm)

Live server: [application.localhost:8001](http://features.localhost:8001)

## Application

Explores very simple Routing.
[[ApplicationMain.elm]](../src/ApplicationMain.elm)

Live server: [application.localhost:8001](http://application.localhost:8001)

## Garden

Walk through the serene Ui Garden and explore what is impossible to build with the `restricted Ui`...
[[Garden.elm]](../src/Garden.elm)

_Work in progress!_

Live server: [garden.localhost:8001](http://garden.localhost:8001)

----

**Credits**: See https://github.com/lydell/elm-watch/tree/main/example for a list of libraries used.