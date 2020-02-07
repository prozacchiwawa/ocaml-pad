(* This line opens the Tea.App modules into the current scope for Program access functions and types *)
open Tea.App

(* This opens the Elm-style virtual-dom functions and types into the current scope *)
open Tea.Html
open Tea.Json

open Basic
open Keyevent
open Runedit

type ocaml
type compileResult

type localStorage

module StringFindMap = FindMap(Runedit.S)
module StringUpdateMap = UpdateMap(Runedit.S)

external ocaml : ocaml = "ocaml" [@@bs.val]
external compile : ocaml -> string -> compileResult = "compile" [@@bs.send]
external compiled_js_code_ : compileResult -> Js.Json.t = "js_code" [@@bs.get]
external compile_error_ : compileResult -> Js.Json.t = "error" [@@bs.get]

external atob : string -> string = "atob" [@@bs.val]
external btoa : string -> string = "btoa" [@@bs.val]

external localStorage : localStorage = "localStorage" [@@bs.val]
external ls_get : localStorage -> string -> string = "getItem" [@@bs.send]
external ls_set : localStorage -> string -> string -> unit = "setItem" [@@bs.send]

let decodeStringArray j : string list =
  j
  |> Js.Json.decodeArray
  |> Option.map (Array.map (Js.Json.decodeString))
  |> Option.map Array.to_list
  |> Option.map (List.map Option.to_list)
  |> Option.map List.concat
  |> Option.else_ (fun _ -> [])

let compiled_js_code res = res |> compiled_js_code_ |> Js.Json.decodeString
let compile_error res =
  res
  |> compile_error_
  |> decodeStringArray

(* Let's create a new type here to be our main message type that is passed around *)
type msg =
  | Nop
  | ToSelectView
  | ToCodeView
  | ToExecView
  | NewProject
  | KeyPress of keyEvent
  | ShowCode of string
  | SaveProgram
  | ExecProgram of string
  [@@bs.deriving {accessors}] (* This is a nice quality-of-life addon from Bucklescript, it will generate function names for each constructor name, optional, but nice to cut down on code, this is unused in this example but good to have regardless *)

type execModel =
  { foo : int
  }

type program =
  { code : string
  ; name : string
  ; errors : string list
  ; output : string list
  }

type editModel =
  { editing : Editing.t
  ; execview : execModel option
  ; program : program
  }

type viewKind =
  | SelectView
  | CodeView
  | ExecView

type model =
  { view : viewKind
  ; codeview : editModel option
  ; programs : program StringMap.t
  ; iframe : (string * string) option
  }

let getProgram name =
  let pobj =
    try
      Js.Json.decodeObject (Js.Json.parseExn (ls_get localStorage ("program:" ^ name)))
    with _ ->
      None
  in
  pobj
  |> Option.bind
    (fun pdict ->
       let name = Js.Dict.get pdict "name" |> Option.bind Js.Json.decodeString in
       let code = Js.Dict.get pdict "code" |> Option.bind Js.Json.decodeString in
       let errors = Js.Dict.get pdict "errors" |> Option.map decodeStringArray in
       let output = Js.Dict.get pdict "output" |> Option.map decodeStringArray in
       match (name, code, errors, output) with
       | (Some name, Some code, Some errors, Some output) ->
         Some { name = name ; code = code ; errors = errors ; output = output }
       | _ -> None
    )

let saveProgram prog =
  let encoded =
    Js.Json.object_
      (Js.Dict.fromList
         [ ("name", Js.Json.string prog.name)
         ; ("code", Js.Json.string prog.code)
         ; ("errors", Js.Json.stringArray (Array.of_list prog.errors))
         ; ("output", Js.Json.stringArray (Array.of_list prog.output))
         ]
      )
  in
  ls_set localStorage ("program:" ^ prog.name) (Js.Json.stringify encoded)

let saveProgramList progs =
  let nameArray =
    StringMap.bindings progs
    |> List.map (fun (k,_) -> k)
    |> Array.of_list
    |> Js.Json.stringArray
  in
  ls_set localStorage "programs" (Js.Json.stringify nameArray)

(* This is optional for such a simple example, but it is good to have an `init` function to define your initial model default values, the model for Counter is just an integer *)
let init () =
  let programNames =
    try
      let parsed = Js.Json.parseExn (ls_get localStorage "programs") in
      decodeStringArray parsed
    with _ ->
      []
  in
  let programs =
    List.map getProgram programNames
    |> List.map Option.to_list
    |> List.concat
    |> List.fold_left (fun m p -> StringMap.add p.name p m) StringMap.empty
  in
  let model =
    { view = SelectView
    ; codeview = None
    ; programs = programs
    ; iframe = None
    }
  in
  model

let passOnKeyPress evt model =
  { model with
    codeview =
      model.codeview
      |> Option.map (fun em -> { em with editing = Editing.accept_key evt em.editing })
  }

let updateProgram name (f : program -> program) model =
  { model with
    programs = StringUpdateMap.go name (Option.map f) model.programs
  ; codeview =
      model.codeview
      |> Option.map
        (fun em ->
           if em.program.name = name then
             { em with program = f em.program }
           else
             em
        )
  }

let saveCurrentProgram model =
  model.codeview
  |> Option.bind
    (fun cv ->
       Editing.to_string cv.program.name cv.editing
       |> Option.map (fun code -> { cv.program with code = code })
    )
  |> Option.map
    (fun (p : program) ->
       let _ = saveProgram p in
       updateProgram p.name (fun _ -> p) model
    )
  |> Option.else_ (fun _ -> model)

(* This is the central message handler, it takes the model as the first argument *)
let update (model : model) = function (* These should be simple enough to be self-explanatory, mutate the model based on the message, easy to read and follow *)
  | Nop -> model
  | KeyPress evt ->
    passOnKeyPress evt model
  | ToSelectView -> { model with view = SelectView }
  | ToCodeView -> { model with view = CodeView }
  | ToExecView -> { model with view = ExecView }
  | NewProject ->
    let newName = Namegen.generateRandomName 4 in
    let program =
      { name = newName
      ; code = "(* New program *)\nlet x = 42\n"
      ; errors = []
      ; output = []
      }
    in
    let _ = saveProgram program in
    let newPrograms = StringMap.add newName program model.programs in
    let _ = saveProgramList newPrograms in
    { model with programs = newPrograms }
  | ShowCode prog ->
    let prog = StringFindMap.go prog model.programs in
    prog
    |> Option.map
      (fun p ->
         let cv =
           { editing = Editing.init 10 10 p.name p.code ; execview = None ; program = p }
         in
         { model with codeview = Some cv ; view = CodeView }
      )
    |> Option.else_ (fun _ -> model)
  | SaveProgram -> saveCurrentProgram model
  | ExecProgram p ->
    let newModel = saveCurrentProgram model in
    StringFindMap.go p newModel.programs
    |> Option.map
      (fun (p : program) ->
         let compilation = compile ocaml p.code in
         let got_code = compiled_js_code compilation in
         let got_errors = compile_error compilation in
         match (got_errors, got_code) with
         | (_, Some code) ->
           let iframecode = "data:text/html;base64," ^ btoa ("<pre>" ^ code ^ "</pre>") in
           updateProgram p.name (fun _ -> { p with output = [] })
             { newModel with
               iframe = Some (p.name, iframecode)
             }
         | (errors, _) ->
           updateProgram p.name (fun _ -> { p with errors = errors }) newModel
         | _ -> model
      )
    |> Option.else_ (fun _ -> model)

(* This is just a helper function for the view, a simple function that returns a button based on some argument *)
let view_button title msg =
  button
    [ onClick msg
    ]
    [ text title
    ]

let viewSelector model =
  let itemDisplay =
    StringMap.bindings model.programs
    |> List.map
      (fun (_,p) ->
         button
           [ classList [ ("select-button", true) ]
           ; onClick (ShowCode p.name)
           ]
           [ text p.name ]
      )
  in
  div [ classList [ ("select-view", true) ] ]
    [ div [ classList [ ("select-heading", true) ] ]
        [ text "Select..."
        ; button [ onClick NewProject ] [ text "+" ]
        ]
    ; div [ classList [ ("select-items", true) ] ]
        itemDisplay
    ]

let runningProgram model = model.iframe |> Option.map (fun (n,_) -> n)

let viewEditor model em =
  div []
    [ div
        [ classList [ ("editor-pane", true) ] ]
        [ Editing.render em.editing
        ; input'
            [ classList [ ("input-in-container", true) ]
            ; onWithOptions ~key:"keydown" "keydown"
                { stopPropagation = true ; preventDefault = true }
                (Keyevent.key_decoder |> Decoder.map (fun a -> KeyPress a))
            ] []
        ]
    ; div [ classList [ ("editor-controls", true) ] ]
        [ button
            [ classList [ ("editor-control-btn", true) ]
            ; onClick SaveProgram
            ] [ text "Save" ]
        ; button
            [ classList [ ("editor-control-btn", true) ]
            ; onClick ToSelectView
            ] [ text "Main" ]
        ; button
            [ classList [ ("editor-control-btn", true) ]
            ; onClick (if runningProgram model <> Some em.program.name then ExecProgram em.program.name else ToExecView)
            ] [ text "Run" ]
        ]
    ]

let viewRun model ev =
  let execEvents = [] in
  div []
    [ div [ classList [ ("exec-title", true) ] ]
        [ button [ classList [ ("exec-title-button", true) ] ; onClick ToCodeView ]
            [ text "Return" ]
        ; text "Run"
        ]
    ; div [ classList [ ("exec-display", true) ] ]
        execEvents
    ]

(* This is the main callback to generate the virtual-dom.
  This returns a virtual-dom node that becomes the view, only changes from call-to-call are set on the real DOM for efficiency, this is also only called once per frame even with many messages sent in within that frame, otherwise does nothing *)
let view model =
  div []
    [ model.iframe
      |> Option.map (fun (name,data) -> iframe [ id name ; src data ] [])
      |> Option.else_ (fun _ -> div [] [])
    ; match model.view with
    | SelectView -> viewSelector model
    | CodeView ->
      model.codeview
      |> Option.map (viewEditor model)
      |> Option.else_ (fun _ -> viewSelector model)
    | ExecView ->
      model.codeview
      |> Option.bind (fun cv -> cv.execview)
      |> Option.map (viewRun model)
      |> Option.else_ (fun _ -> viewSelector model)
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
