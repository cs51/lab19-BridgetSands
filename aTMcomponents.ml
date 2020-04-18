open Printf ;;
open Scanf ;;


module DB = Database

type id = int ;;

type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;;

type account_spec = {name : string; id : id; balance : int} ;;


let rec initialize (a : account_spec list) : unit =
  a
  |> List.iter (fun {name; id; balance}
                -> DB.create id name;
                   DB.update id balance) ;;


let rec acquire_id () : id =
  printf "Enter ID: ";
  try
    let id = read_int () in
    ignore (DB.exists id); id
  with
  | Not_found
  | Failure _ -> printf "Invalid id \n";
                 aqurire_id () ;;

let rec acquire_amount () : int =
  try
    printf "Enter Amount: ";
    let amt = read_int () in
    if amt <= 0 then raise (Failure "amount is negative"); amt
  with
  | Failure _ -> printf "Invalid amount \n";
                 acquire_amount () ;;

let rec acquire_act (u : unit) : action =
  Printf.printf "Enter Action: balance, next, finished, withdraw, deposit: ";
  let a = String.lowercase_ascii (read_string u) in
  match a with
  | "balance" -> Balance
  | "next" -> Next
  | "finished" -> Finished
  | "withdraw" -> Withdraw (acquire_amount ())
  | "deposit" -> Deposit (acquire_amount ())
  | _ -> printf "invalid choice"; acquire_act () ;;

  let get_balance : id -> int = DB.balance ;;

  let get_name : id -> string = DB.name ;;

  let update_balance : id -> int -> unit = DB.update ;;

  let present_message (m : string) : unit =
    printf "%s\n%!" m ;;

  let deliver_cash (amount : int) : unit =
    printf "CASH: ";
    for _i = 1 to (amount / 20) do
      printf "[20 @ 20]"
    done;
      printf " and %d more\n" (amount mod 20) ;;
