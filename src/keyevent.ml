open Tea.Json

type keyEvent =
  { ctrlKey : bool
  ; altKey : bool
  ; shiftKey : bool
  ; key : string
  ; keyCode : int
  }

let key_decoder =
  (Decoder.map5
     (fun ctrl alt shift kstr code ->
        { ctrlKey = ctrl
        ; altKey = alt
        ; shiftKey = shift
        ; key = kstr
        ; keyCode = code
        }
     )
     (Decoder.field "ctrlKey" Decoder.bool)
     (Decoder.field "altKey" Decoder.bool)
     (Decoder.field "shiftKey" Decoder.bool)
     (Decoder.field "key" Decoder.string)
     (Decoder.field "keyCode" Decoder.int)
  )
