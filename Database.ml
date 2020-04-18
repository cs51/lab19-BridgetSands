type id = int ;;

module DataB = Map.Make (struct
                            type t = int
                            let compare : int -> int -> int = compare
                          end )  ;;

let database: (string * int) DataB.t ref = ref DataB.empty ;;

let create (id : id) (n : string) : unit =
  database := DataB.add id (n, 0) !database ;;

let find (id : id ) : string * int =
  DataB.find id !database ;;

let exists (id : id) : bool =
  DataB.exists (fun key _val -> key = id) !database ;;

let balance (id : id ) : int =
  let _n, amt = find id in
  amt ;;

let name (id : id ) : string =
  let n, _amt = find id in
  n ;;

let update (id : id) (amt : int) =
  let n = name id in
  database := DataB.add id (n, amt) !database ;;

let close (id : id) : unit =
  database := DataB.remove id !database ;;

let dump () =
  !database
  |> DataB.iter (fun x (n, amt) ->
                 Printf.printf "[%d] %s -> %d\n" i n amt ) ;;
