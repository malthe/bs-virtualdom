external unsafe_identity : 'a -> 'b = "%identity"

let empty = [||]

let ( >>? ) v g =
  match v with
  | Some v -> g v |> ignore
  | None -> ()

let ( >>! ) v g =
  match v with
  | Some v -> g v
  | None -> None

let ( =| ) s default = match s with
  | Some s -> Some s
  | None -> default

let fold_lefti f a b =
  Array.fold_left (fun (i, a) b -> (i + 1, f a i b)) (0, a) b |> snd

let strictly_equal_to a b =
  let f : 'a -> 'a -> bool = [%raw fun a b -> "{return a === b}"] in
  f a (unsafe_identity b)
