module Tools.Control exposing (interface)

import Control



-- Do not infer this type! --


interface =
    { string = Control.string
    , int = Control.int
    , bool = Control.bool
    , float = Control.float
    , char = Control.char
    , list = Control.list
    , maybe = Control.maybe
    , array = Control.array
    , dict = Control.dict
    , set = Control.set
    , tuple = Control.tuple
    , triple = Control.triple
    , result = Control.result
    , record = Control.record
    , field =
        -- Control.field doesn't use the field name
        \_ getter subControl -> Control.field getter subControl
    , endRecord = Control.endRecord
    , custom = Control.customType
    , tag0 = Control.tag0
    , tag1 = Control.tag1
    , tag2 = Control.tag2
    , tag3 = Control.tag3
    , tag4 = Control.tag4
    , tag5 = Control.tag5
    , endCustom = Control.endCustomType
    }
