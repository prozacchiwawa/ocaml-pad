module type Ord = sig
  type t
  val compare : t -> t -> int
end

let flip f a b = f b a

module Option = struct
  let map f v =
    match v with
    | Some o -> Some (f o)
    | None -> None

  let bind f v =
    match v with
    | Some o -> f o
    | _ -> None

  let else_ f v =
    match v with
    | Some o -> o
    | None -> f ()

  let to_list v =
    match v with
    | Some v -> [v]
    | _ -> []
end

module FindMap (O : Ord) = struct
  module MapT = Map.Make(O)
  let go (key : MapT.key) (map : 'a MapT.t) : 'a option =
    try
      Some (MapT.find key map)
    with _ -> None
end
module UpdateMap (O : Ord) = struct
  module MapT = Map.Make(O)
  module Find = FindMap(O)
  let go (key : MapT.key) (f : 'a option -> 'a option) (map : 'a MapT.t) =
    let res = Find.go key map in
    match f res with
    | Some a -> MapT.remove key map |> MapT.add key a
    | None -> MapT.remove key map
end
