module Restrictive.Msg exposing (Msg(..))

{-|

@docs Msg

-}

import Browser
import Restrictive.State exposing (UrlCmd)
import Url exposing (Url)


{-| Handles your message type (`modelMsg`) as well as changes to the Ui state (Url).
-}
type Msg modelMsg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | ModelMsg modelMsg
    | UrlCmds (List UrlCmd)
