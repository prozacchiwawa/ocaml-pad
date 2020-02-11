open Tea.Json
open Tea.Html

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

let keyset0 =
  [ ("keyboard-top-row", "1234567890", "!@#$%^&*()")
  ; ("keyboard-top-row", "qwertyuiop", "QWERTYUIOP")
  ; ("keyboard-mid-row", "`asdfghjkl", "~ASDFGHJKL")
  ; ("keyboard-bot-row", "\x09zxcvbnm\x0d", "\x09ZXCVBNM\x0d")
  ; ("keyboard-space-row", "\x01' -=\x01", "\x01\" _+\x01")
  ; ("keyboard-space-row", ";<>?[]\\\x08", ":,./{}|\x08")
  ]

let softKeyboard shifted event keyset =
  div
    [ classList [ ("keyboard-container",true) ] ]
    (keyset |> List.map
       (fun (classname, unshiftedRow, shiftedRow) ->
          let row = if shifted then shiftedRow else unshiftedRow in
          let keysRow = List.init (String.length row) (fun i -> String.get row i) in
          div [ classList [ (classname, true) ; ("keyboard-row",true) ] ]
            (keysRow |> List.mapi
               (fun i ch ->
                  let cap = String.make 1 ch in
                  let capstr =
                    if cap = "\x08" then
                      "BACKSP"
                    else if cap = "\x0d" then
                      "RETN"
                    else if cap = "\x09" then
                      "TB"
                    else if cap = "\x01" then
                      "SH"
                    else if cap = " " then
                      "~ ~ ~ ~ SPACE ~ ~ ~ ~"
                    else
                      cap
                  in
                  button [ classList [ ("keycap",true) ] ; onClick (event unshiftedRow shiftedRow i) ] [ text capstr ]
               )
            )
       )
    )
