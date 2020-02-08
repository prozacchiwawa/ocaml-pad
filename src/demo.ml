(* This line opens the Tea.App modules into the current scope for Program access functions and types *)
open Tea.App

(* This opens the Elm-style virtual-dom functions and types into the current scope *)
open Tea.Html
open Tea.Json

open Basic
open Keyevent
open Runedit
open Resize

type ocaml
type compileResult

type localStorage

module StringFindMap = FindMap(Runedit.S)
module StringUpdateMap = UpdateMap(Runedit.S)

external ocaml : ocaml = "ocaml" [@@bs.val]
external compile : ocaml -> string -> compileResult = "compile" [@@bs.send]
external compiled_js_code_ : compileResult -> Js.Json.t = "js_code" [@@bs.get]
external compile_error_ : compileResult -> Js.Json.t = "js_error_msg" [@@bs.get]

external atob : string -> string = "atob" [@@bs.val]
external btoa : string -> string = "btoa" [@@bs.val]

external localStorage : localStorage = "localStorage" [@@bs.val]
external ls_get : localStorage -> string -> string = "getItem" [@@bs.send]
external ls_set : localStorage -> string -> string -> unit = "setItem" [@@bs.send]

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
  | StartedProgram
  | OutputLine of string
  | SizeReport of resizeData
  | DeleteProgram of string

  | Home
  | End
  | LeftArrow
  | RightArrow
  | DownArrow
  | UpArrow
  [@@bs.deriving {accessors}] (* This is a nice quality-of-life addon from Bucklescript, it will generate function names for each constructor name, optional, but nice to cut down on code, this is unused in this example but good to have regardless *)

type shareData

external shareData : shareData = "window.shareData" [@@bs.val]
external setMsgEncoder : shareData -> (Js.Json.t -> msg) -> unit = "setMsgEncoder" [@@bs.send]
external pokeProgram : shareData -> string -> string -> unit = "pokeProgram" [@@bs.send]
external resizeWindow : shareData -> string -> unit = "resizeWindow" [@@bs.send]

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
  |> Js.Json.decodeString

type program =
  { code : string
  ; name : string
  ; errors : string
  ; output : string list
  }

type editModel =
  { editing : Editing.t
  ; program : program
  }

type viewKind =
  | SelectView
  | CodeView
  | ExecView

type iframeData =
  { if_id : string
  ; if_src : string
  ; if_code : string
  }

type model =
  { view : viewKind
  ; codeview : editModel option
  ; programs : program StringMap.t
  ; iframe : iframeData option
  ; runcount : int
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
       let errors = Js.Dict.get pdict "errors" |> Option.bind Js.Json.decodeString in
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
         ; ("errors", Js.Json.string prog.errors)
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
    ; runcount = 0
    }
  in
  let _ =
    setMsgEncoder shareData
      (fun d ->
         let obj = Js.Json.decodeObject d in
         let message =
           obj
           |> Option.bind (fun o -> Js.Dict.get o "message")
           |> Option.bind Js.Json.decodeString
         in
         let data =
           obj
           |> Option.bind (fun o -> Js.Dict.get o "data")
           |> Option.bind Js.Json.decodeString
         in
         let rect = obj |> Option.bind decodeSizeReport in
         match (message, data, rect) with
         | (Some "started", _, _) -> StartedProgram
         | (Some "output", Some value, _) -> OutputLine value
         | (Some "size", _, Some size) ->
           let rpt = SizeReport size in
           let _ = Js.log rpt in
           rpt
         | _ -> Nop
      )
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

let passOnKeyCode code model =
  let event =
    { ctrlKey = false
    ; altKey = false
    ; shiftKey = false
    ; key = ""
    ; keyCode = code
    }
  in
  passOnKeyPress event model


(* This is the central message handler, it takes the model as the first argument *)
let update (model : model) = function (* These should be simple enough to be self-explanatory, mutate the model based on the message, easy to read and follow *)
  | Nop -> model
  | KeyPress evt ->
    passOnKeyPress evt model
  | ToSelectView ->
    let newModel = saveCurrentProgram model in
    { newModel with view = SelectView }
  | ToCodeView -> { model with view = CodeView }
  | ToExecView -> { model with view = ExecView }
  | NewProject ->
    let newName = Namegen.generateRandomName 4 in
    let program =
      { name = newName
      ; code = "(* New program *)\nlet print_endline : string -> unit = [%bs.raw {| function(s) { window.parent.postMessage({'message':'output','data':s}, '*'); } |} ]\nlet x = 42\n"
      ; errors = ""
      ; output = []
      }
    in
    let _ = saveProgram program in
    let newPrograms = StringMap.add newName program model.programs in
    let _ = saveProgramList newPrograms in
    { model with programs = newPrograms }
  | ShowCode prog ->
    let _ = resizeWindow shareData "editor" in
    let prog = StringFindMap.go prog model.programs in
    prog
    |> Option.map
      (fun p ->
         let cv =
           { editing = Editing.init 25 13 p.name p.code ; program = p }
         in
         { model with codeview = Some cv ; view = CodeView }
      )
    |> Option.else_ (fun _ -> model)
  | SaveProgram -> saveCurrentProgram model
  | ExecProgram p ->
    let newModel' = saveCurrentProgram model in
    let newModel = { newModel' with runcount = newModel'.runcount + 1 } in
    StringFindMap.go p newModel.programs
    |> Option.map
      (fun (p : program) ->
         let compilation = compile ocaml p.code in
         let got_code = compiled_js_code compilation in
         let got_errors = compile_error compilation in
         match (got_errors, got_code) with
         | (_, Some code) ->
           (* Thanks : https://michelenasti.com/2018/10/02/let-s-write-a-simple-version-of-the-require-function.html *)
           let iframecode = "data:text/html;base64," ^ btoa ("<script>window.parent.postMessage({'message':'started'}, '*'); window.addEventListener('message', function(m) { if(m.data.message === 'runme') { eval(m.data.code); } });</script>")
           in
           let pokeCode = "exports = {};\n" ^ Libs.jslibs ^ "var _runcount = " ^ (string_of_int (model.runcount)) ^ "; require.cache = Object.create(null); //(1)\nfunction require(name) {\nif (!(name in require.cache)) {\nlet code = imports[name]; //(2)\nlet module = {exports: {}}; //(3)\nrequire.cache[name] = module; //(4)\nlet wrapper = Function(\"require, exports, module\", code); //(5)\nwrapper(require, module.exports, module); //(6)\n}\nreturn require.cache[name].exports; //(7)\n}" ^ code
           in
           let _ = pokeProgram shareData p.name pokeCode in
           updateProgram p.name (fun _ -> { p with output = [] ; errors = "" })
             { newModel with
               view = ExecView
             ; iframe =
                 Some
                   { if_id = p.name
                   ; if_src = iframecode
                   ; if_code = pokeCode
                   }
             }

         | (Some errors, _) ->
           let iframecode = "data:text/html;base64," ^ btoa ("<pre>" ^ errors ^ "</pre>") in
           updateProgram p.name (fun _ -> { p with output = [] ; errors = errors })
             { newModel with
               iframe = Some { if_id = p.name ; if_src = iframecode ; if_code = "" }
             }

         | _ -> model
      )
    |> Option.else_ (fun _ -> model)
  | StartedProgram ->
    model.iframe
    |> Option.map
      (fun iframe ->
         let _ = pokeProgram shareData iframe.if_id iframe.if_code in
         model
      )
    |> Option.else_ (fun _ -> model)
  | OutputLine o ->
    model.iframe
    |> Option.map
      (fun iframe ->
         updateProgram iframe.if_id (fun p -> { p with output = o :: p.output }) model
      )
    |> Option.else_ (fun _ -> model)
  | SizeReport rpt ->
    model.codeview
    |> Option.map
      (fun cv ->
         { model with codeview = Some { cv with editing = Editing.resize rpt cv.editing } }
      )
    |> Option.else_ (fun _ -> model)
  | End -> passOnKeyCode 35 model
  | Home -> passOnKeyCode 36 model
  | LeftArrow -> passOnKeyCode 37 model
  | UpArrow -> passOnKeyCode 38 model
  | RightArrow -> passOnKeyCode 39 model
  | DownArrow -> passOnKeyCode 40 model

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
        [ text "Programs"
        ; button [ classList [ ("plusbut",true) ] ; onClick NewProject ] [ ]
        ]
    ; div [ classList [ ("select-items", true) ] ]
        itemDisplay
    ]

let runningProgram model = model.iframe |> Option.map (fun iframe -> iframe.if_id)

let synthesizeKey t =
  KeyPress
    { ctrlKey = false
    ; altKey = false
    ; shiftKey = false
    ; key = String.sub t ((String.length t) - 1) 1
    ; keyCode = 0
    }

let viewEditor model em =
  div []
    [ div [ classList [ ("editor-controls-container", true) ] ]
        [ div [ classList [ ("editor-controls", true) ] ]
            [ button
                [ classList [ ("ebut",true) ; ("editor-control-btn", true) ]
                ; onClick SaveProgram
                ] [ text "Save" ]
            ; button
                [ classList [ ("ebut",true) ; ("editor-control-btn", true) ]
                ; onClick (ExecProgram em.program.name)
                ] [ text "Run" ]
            ]
        ]
    ; div
        [ classList [ ("editor-pane", true) ] ]
        [ Editing.render em.editing
        ; input'
            [ classList [ ("input-in-container", true) ]
            ; type' "text"
            ; value ""
            ; Vdom.prop "pattern" "^$"
            ; Vdom.prop "readonly" "true"
            ; Vdom.prop "maxlength" "0"
            ; Vdom.prop "autocomplete" "off"
            ; onWithOptions ~key:"keydown" "keydown"
                { stopPropagation = true ; preventDefault = true }
                (Keyevent.key_decoder |> Decoder.map (fun a -> KeyPress a))
            ; onInput (fun t -> synthesizeKey t)
            ] []
        ]
    ; div [ classList [ ("editor-controls-container",true) ] ]
        [ div [ classList [ ("editor-controls", true) ] ]
            [ button [ classList [ ("kbut",true) ; ("homebut", true) ] ; onClick Home ] [ ]
            ; button [ classList [ ("kbut",true) ; ("leftbut", true) ] ; onClick LeftArrow ] [ ]
            ; button [ classList [ ("kbut",true) ; ("upbut", true) ] ; onClick UpArrow ] [ ]
            ; button [ classList [ ("kbut",true) ; ("downbut", true) ] ; onClick DownArrow ] [ ]
            ; button [ classList [ ("kbut",true) ; ("rightbut", true) ] ; onClick RightArrow ] [ ]
            ; button [ classList [ ("kbut",true) ; ("endbut", true) ] ; onClick End ] [ ]
            ]
        ]
    ; div [ classList [ ("editor-errors", true) ] ]
        [ text em.program.errors ]
    ]

let viewRun model ev =
  let execEvents =
    model.iframe
    |> Option.map (fun iframe -> iframe.if_id)
    |> Option.bind (fun id -> StringFindMap.go id model.programs)
    |> Option.map (fun p -> p.output)
    |> Option.else_ (fun _ -> [])
  in
  div []
    [ div [ classList [ ("exec-title", true) ] ]
        [ text ("Running ... " ^ ev.program.name)
        ]
    ; div [ classList [ ("exec-display", true) ] ]
        (execEvents |> List.map (fun line -> div [] [ text line ]) |> List.rev)
    ]

let viewBreadcrumbs model =
  let editor_showing = model.codeview <> None in
  let runner_showing = model.iframe <> None in
  div [ classList [ ("nav-container", true) ] ]
    [ div [ classList [ ("nav-list", true) ] ]
        [ button
            [ classList [ ("nav-button", true) ; ("nav-enabled", true) ]
            ; onClick ToSelectView
            ]
            [ text "Main" ]
        ; button
            [ classList [ ("nav-button", true) ; ("nav-enabled", editor_showing) ]
            ; onClick ToCodeView
            ]
            [ text "Editor" ]
        ; button
            [ classList [ ("nav-button", true) ; ("nav-enabled", runner_showing) ]
            ; onClick ToExecView
            ]
            [ text "Runner" ]
        ]
    ]

(* This is the main callback to generate the virtual-dom.
  This returns a virtual-dom node that becomes the view, only changes from call-to-call are set on the real DOM for efficiency, this is also only called once per frame even with many messages sent in within that frame, otherwise does nothing *)
let view model =
  div []
    [ model.iframe
      |> Option.map (fun ifdata -> iframe [ id ifdata.if_id ; src ifdata.if_src ; classList [ ("iframe-style", true) ] ] [])
      |> Option.else_ (fun _ -> div [] [])
    ; div []
        [ viewBreadcrumbs model
        ; match model.view with
          | SelectView -> viewSelector model
          | CodeView ->
            model.codeview
            |> Option.map (viewEditor model)
            |> Option.else_ (fun _ -> viewSelector model)
          | ExecView ->
            model.codeview
            |> Option.map (viewRun model)
            |> Option.else_ (fun _ -> viewSelector model)
        ]
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
