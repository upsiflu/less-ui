# v2

- Added `Less.reroute` and `Less.Link.mapLocation`
- Changed the Api of `Less.application` to not need a State transition

```elm
myReroute : Less.State -> Less.State
myReroute =
    Less.Link.mapLocation
        (\location -> if location == "/" then "/newHomePage" else location)

myView : () -> Document msg
myView () =
    Less.reroute myReroute >> myDocument
```

- Replaced `Less.Ui.Html.block` and `Less.Ui.Html.inline` with `Less.Ui.Html.node`
