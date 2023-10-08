# Unreleased

[f670e8d](https://github.com/upsiflu/less-ui/commit/f670e8d35682c872e1845238a1eb7ab60629558c)


```elm
myReroute : Less.State -> Less.State
myReroute =
    Less.Link.mapLocation
        (\location -> if location == "/" then "/newHomePage" else location)

myView : () -> Document msg
myView () =
    Less.reroute myReroute >> myDocument
```

## Api

#### Less

**+** `reroute`

#### Less.Link

**+** `mapLocation` 

####Less.Ui.Html

`block` &rarr; `node`

`inline` &rarr; `node`
