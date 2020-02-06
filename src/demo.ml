(* This line opens the Tea.App modules into the current scope for Program access functions and types *)
open Tea.App

(* This opens the Elm-style virtual-dom functions and types into the current scope *)
open Tea.Html
open Runedit

type ocaml
type compileResult

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

external ocaml : ocaml = "ocaml" [@@bs.val]
external compile : ocaml -> string -> compileResult = "compile" [@@bs.send]
external compiled_js_code_ : compileResult -> Js.Json.t = "js_code" [@@bs.get]
external compile_error_ : compileResult -> Js.Json.t = "error" [@@bs.get]

let compiled_js_code res = res |> compiled_js_code_ |> Js.Json.decodeString
let compile_error res = res |> compile_error_ |> Js.Json.decodeString

let flip f a b = f b a

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

type keyEvent
external ctrlKey : keyEvent -> bool = "ctrlKey" [@@bs.send]
external altKey : keyEvent -> bool = "altKey" [@@bs.send]
external shiftKey : keyEvent -> bool = "shiftKey" [@@bs.send]
external key : keyEvent -> string = "key" [@@bs.send]
external keyCode : keyEvent -> int = "keyCode" [@@bs.send]

let mapKey evt =
  let startState =
    { ctrl = ctrlKey evt
    ; alt = altKey evt
    ; shift = shiftKey evt
    ; s =
        try
          StringMap.find (key evt) keymap
        with _ ->
          "letter_" ^ (String.uppercase_ascii (key evt))
    }
  in
  let handleDigit state =
    if String.length (key evt) == 1 && (key evt) >= "0" && (key evt) <= "9" then
      { state with s = "digit_" ^ (key evt) }
    else
      state
  in
  let handleLetter state =
    if String.length (key evt) == 1 && ((key evt) >= "A" && (key evt) <= "Z") then
      { state with s = "letter_" ^ (String.uppercase_ascii (key evt)) ; shift = false }
    else if String.length (key evt) == 1 && ((key evt) >= "a" && (key evt) <= "z") then
      { state with s = "letter_" ^ (String.lowercase_ascii (key evt)) ; shift = false }
    else
      state
  in
  let handleCode state =
    try
      { state with s = StringMap.find ("code_" ^ (string_of_int (keyCode evt))) keymap }
    with _ ->
      state
  in
  let handleFKey state =
    if (keyCode evt) >= 112 && (keyCode evt) < 112 + 12 then
      try
        { state with s = StringMap.find ("f" ^ (string_of_int ((keyCode evt) - 111))) keymap }
      with _ ->
        state
    else
      state
  in
  let handleCtrlKey state =
    if String.length (key evt) == 1 then
      let theCode = Char.code (String.get (key evt) 0) in
      if theCode < 32 then
        { state with s = String.make 1 (Char.chr (theCode + 96)) }
      else
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
  |> handleCtrlKey
  |> finalizeKey

(* Let's create a new type here to be our main message type that is passed around *)
type msg =
  | Increment  (* This will be our message to increment the counter *)
  | Decrement  (* This will be our message to decrement the counter *)
  | Reset      (* This will be our message to reset the counter to 0 *)
  | Set of int (* This will be our message to set the counter to a specific value *)
  [@@bs.deriving {accessors}] (* This is a nice quality-of-life addon from Bucklescript, it will generate function names for each constructor name, optional, but nice to cut down on code, this is unused in this example but good to have regardless *)

type model =
  { editor : State.t
  ; frame : Render.frame
  }

(* This is optional for such a simple example, but it is good to have an `init` function to define your initial model default values, the model for Counter is just an integer *)
let init () =
  let model =
    { editor = Main.edit "hi.ml" "(* Hi there *)\nlet x = 42\n"
    ; frame = Render.new_frame 80 40
    }
  in
  let _ = Render.render model.frame in
  model

(* This is the central message handler, it takes the model as the first argument *)
let update model = function (* These should be simple enough to be self-explanatory, mutate the model based on the message, easy to read and follow *)
  | Increment -> model
  | Decrement -> model
  | Reset -> model
  | Set v -> model

(* This is just a helper function for the view, a simple function that returns a button based on some argument *)
let view_button title msg =
  button
    [ onClick msg
    ]
    [ text title
    ]

type drawAccum =
  { lastStyle : Style.t
  ; currentStr : string
  ; results : (Style.t * string) list
  }

module IntIntSet = Set.Make(II)

let renderEditor model =
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
  div [] (Array.to_list resultDivs)

(* This is the main callback to generate the virtual-dom.
  This returns a virtual-dom node that becomes the view, only changes from call-to-call are set on the real DOM for efficiency, this is also only called once per frame even with many messages sent in within that frame, otherwise does nothing *)
let view model =
  div
    []
    [ renderEditor model
    ]

(* This is the main function, it can be named anything you want but `main` is traditional.
  The Program returned here has a set of callbacks that can easily be called from
  Bucklescript or from javascript for running this main attached to an element,
  or even to pass a message into the event loop.  You can even expose the
  constructors to the messages to javascript via the above [@@bs.deriving {accessors}]
  attribute on the `msg` type or manually, that way even javascript can use it safely. *)
let main =
  beginnerProgram { (* The beginnerProgram just takes a set model state and the update and view functions *)
    model = init (); (* Since model is a set value here, we call our init function to generate that value *)
    update;
    view;
  }
