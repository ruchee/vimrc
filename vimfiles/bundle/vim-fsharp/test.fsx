let (|Item|_|) = Map.tryFind

let m = Map ["key1", 1; "key2", 2]

let withAp m k =
    match m with
    | Item k v -> Some v
    | _ -> None

let without m k =
    match Map.tryFind k m with
    | Some v ->
       Some v
    | None ->
        None





