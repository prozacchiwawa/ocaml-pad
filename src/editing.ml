open Tea.Html

open Runedit
open Keyevent
open Resize

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type t =
  { editor : State.t
  ; frame : Render.frame
  ; lastkey : string
  }

let keymap =
  List.fold_left
    (fun m (k,v) -> StringMap.add k v m)
    StringMap.empty
    [ ("\x08", "backspace")
    ; ("\x09", "tab")
    ; ("\x0d", "return")
    ; ("\x1b", "escape")
    ; ("Enter", "return")
    ; (" ", "space")
    ; ("!", "exclamation_mark")
    ; ("\"", "double_quote")
    ; ("#", "hash")
    ; ("$", "dollar")
    ; ("%", "percent")
    ; ("&", "amersand")
    ; ("'", "quote")
    ; ("(", "left_parenthesis")
    ; (")", "right_parenthesis")
    ; ("_", "underscore")
    ; ("`", "backquote")
    ; ("*", "star")
    ; ("^", "caret")
    ; ("+", "plus")
    ; (",", "comma")
    ; ("-", "minus")
    ; (".", "period")
    ; ("\\", "antislash")
    ; ("/", "slash")
    ; (":", "colon")
    ; (";", "semicolon")
    ; ("<", "left_chevron")
    ; ("=", "equal")
    ; (">", "right_chevron")
    ; ("?", "question_mark")
    ; ("{", "left_brace")
    ; ("|", "pipe")
    ; ("}", "right_brace")
    ; ("[", "left_bracket")
    ; ("]", "right_bracket")
    ; ("~", "tilde")
    ; ("ArrowRight", "right")
    ; ("ArrowLeft", "left")
    ; ("ArrowUp", "up")
    ; ("ArrowDown", "down")
    ; ("code_33", "page_up")
    ; ("code_34", "page_down")
    ; ("code_35", "end")
    ; ("code_36", "home")
    ; ("code_37", "left")
    ; ("code_38", "up")
    ; ("code_39", "right")
    ; ("code_40", "down")
    ; ("code_45", "insert")
    ; ("code_46", "delete")
    ]

let cancelShift = StringSet.of_list
    [ "exclamation_mark"
    ; "double_quote"
    ; "hash"
    ; "dollar"
    ; "percent"
    ; "amersand"
    ; "left_parenthesis"
    ; "right_parenthesis"
    ; "underscore"
    ; "backquote"
    ; "star"
    ; "plus"
    ; "comma"
    ; "minus"
    ; "period"
    ; "slash"
    ; "caret"
    ; "colon"
    ; "semicolon"
    ; "left_chevron"
    ; "equal"
    ; "right_chevron"
    ; "question_mark"
    ; "pipe"
    ; "tilde"
    ; "antislash"
    ; "left_brace"
    ; "leff_bracket"
    ; "right_brace"
    ; "right_bracket"
    ]

type keyState =
  { ctrl : bool
  ; alt : bool
  ; shift : bool
  ; s : string
  }

let mapKey evt =
  let startState =
    { ctrl = evt.ctrlKey
    ; alt = evt.altKey
    ; shift = evt.shiftKey
    ; s =
        try
          StringMap.find (evt.key) keymap
        with _ ->
          evt.key
    }
  in
  let handleDigit state =
    if String.length (evt.key) == 1 && (evt.key) >= "0" && (evt.key) <= "9" then
      { state with s = "digit_" ^ (evt.key) }
    else
      state
  in
  let handleLetter state =
    if String.length (evt.key) == 1 && ((evt.key) >= "A" && (evt.key) <= "Z") then
      { state with s = "letter_" ^ (String.uppercase_ascii (evt.key)) ; shift = false }
    else if String.length (evt.key) == 1 && ((evt.key) >= "a" && (evt.key) <= "z") then
      { state with s = "letter_" ^ (String.lowercase_ascii (evt.key)) ; shift = false }
    else
      state
  in
  let handleCode state =
    try
      { state with s = StringMap.find ("code_" ^ (string_of_int (evt.keyCode))) keymap }
    with _ ->
      state
  in
  let handleFKey state =
    if (evt.keyCode) >= 112 && (evt.keyCode) < 112 + 12 then
      try
        { state with s = StringMap.find ("f" ^ (string_of_int ((evt.keyCode) - 111))) keymap }
      with _ ->
        state
    else
      state
  in
  let finalizeKey state =
    let shifted =
      if state.shift then
        "shift_" ^ state.s
      else
        state.s
    in
    let alted =
      if state.alt then
        "alt_" ^ shifted
      else
        shifted
    in
    let ctrled =
      if state.ctrl then
        "ctrl_" ^ alted
      else
        alted
    in
    let first = String.sub state.s 0 1 in
    let rest = String.sub state.s 1 ((String.length state.s) - 1) in
    (String.uppercase_ascii first) ^ rest
  in
  startState
  |> handleDigit
  |> handleLetter
  |> handleCode
  |> handleFKey
  |> finalizeKey

let accept_key evt editing =
  let red_input = mapKey evt in
  let finalKey =
    try
      Some (Runedit.StringMap.find red_input Runedit.keyMap)
    with _ ->
      None
  in
  match finalKey with
  | Some k ->
    let _ = ignore (State.on_key_press editing.editor k) in
    { editing with lastkey = Key.show k }
  | _ -> editing

let init x y name text =
  let e =
    { editor = Main.edit name text
    ; frame = Render.new_frame x y
    ; lastkey = ""
    }
  in
  let _ = Render.render e.frame in
  e

type drawAccum =
  { lastStyle : Style.t
  ; currentStr : string
  ; results : (Style.t * string) list
  }

let render model =
  let x = Render.width model.frame in
  let y = Render.height model.frame in
  let view = State.get_focused_view model.editor in
  let cursors = view.cursors in
  let events = State.render model.editor model.frame in
  let divs = Array.make y () in
  let cols = Array.init x (fun i -> i) in

  let partitionPlane i da j =
    let currentStyle = Array.get model.frame.Render.styles (i * x + j) in
    let nextString = Array.get model.frame.Render.chars (i * x + j) in
    if currentStyle = da.lastStyle then
      { da with currentStr = da.currentStr ^ nextString }
    else
      { da with
        lastStyle = currentStyle ;
        currentStr = nextString ;
        results = (da.lastStyle, da.currentStr) :: da.results
      }
  in
  let finish da =
    let r =
      if da.currentStr <> "" then
        (da.lastStyle, da.currentStr) :: da.results
      else
        da.results
    in
    List.rev r
  in
  let fgColor = function
    | Term.Default -> "default"
    | Term.Black -> "black"
    | Term.Red -> "red"
    | Term.Green -> "green"
    | Term.Yellow -> "yellow"
    | Term.Blue -> "blue"
    | Term.Magenta -> "magenta"
    | Term.Cyan -> "cyan"
    | Term.White -> "white"
  in
  let bgColor = fgColor in
  let resultDivs =
    divs |> Array.mapi
      (fun i _ ->
         let result =
           Array.fold_left
             (partitionPlane i)
             { lastStyle = Array.get model.frame.Render.styles (i * x)
             ; currentStr = ""
             ; results = []
             }
             cols
         in
         let finalRes = finish result in
         div [ classList [ ("editor-row", true) ] ] (List.map (fun (st,txt) -> pre [ classList [ ("tseg", true) ; ("bg-" ^ (bgColor st.Style.bg)), true ; ("fg-" ^ (fgColor st.Style.fg)), true ] ] [ text txt ]) finalRes)
      )
  in
  div [ id "editor" ; classList [ ("editor", true) ] ]
    (List.concat
       [ Array.to_list resultDivs
       (*       ; [ text model.lastkey ] *)
       ]
    )

let resize rpt editing =
  let fontWidth = rpt.editorX / editing.frame.planeWidth in
  let fontHeight = rpt.editorY in
  let x = rpt.windowX / fontWidth in
  let y = rpt.windowY / fontHeight in
  let e =
    { editing with
      editor = editing.editor
    ; frame = Render.new_frame x y
    }
  in
  let _ = Render.render e.frame in
  e

let to_string name editing =
  match List.filter (fun (f : File.t) -> f.name = name) editing.editor.files with
  | [f] -> Some (Text.to_string f.text)
  | _ -> None
