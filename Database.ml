type id = int ;;

module DataB = Map.Make (struct
                            type t = int
                            let compare : int -> int -> int = compare
                          end )  ;;

let data_base: (string * int) DataB.t ref = ref DataB.empty ;;

let create (id : id) (n : string) : unit =
  data_base := DataB.add id (n, 0) !data_base ;;

let find (id : id ) : string * int =
  DataB.find id !data_base ;;

let exists (id : id) : bool =
  DataB.exists (fun key _val -> key = id) !data_base ;;

let balance (id : id ) : int =
  let _n, amt = find id in
  amt ;;

let name (id : id ) : string =
  let n, _amt = find id in
  n ;;

let update (id : id) (amt : int) =
  let n = name id in
  data_base := DataB.add id (n, amt) !data_base ;;

let close (id : id) : unit =
  data_base := DataB.remove id !data_base ;;
