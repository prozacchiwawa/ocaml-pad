open Basic

type resizeData =
  { windowX : int
  ; windowY : int
  ; editorX : int
  ; editorY : int
  }

let decodeSizeReport o =
  let mt =
    ( Js.Dict.get o "windowX" |> Option.bind Js.Json.decodeNumber |> Option.map int_of_float
    , Js.Dict.get o "windowY" |> Option.bind Js.Json.decodeNumber |> Option.map int_of_float
    , Js.Dict.get o "editorX" |> Option.bind Js.Json.decodeNumber |> Option.map int_of_float
    , Js.Dict.get o "editorY" |> Option.bind Js.Json.decodeNumber |> Option.map int_of_float
    )
  in
  match mt with
  | (Some wx, Some wy, Some ex, Some ey) ->
    Some { windowX = wx ; windowY = wy ; editorX = ex ; editorY = ey }
  | _ -> None
