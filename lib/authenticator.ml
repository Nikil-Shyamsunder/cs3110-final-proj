(* Helper function to split a string by commas *)
let split_by_comma line =
  let parts = String.split_on_char ',' line in
  match parts with
  | [ username; password; role; data ] -> (username, password, role, data)
  | _ -> failwith "Malformed CSV line"

(* Load users from CSV file *)
let load_users filename =
  let ic = open_in filename in
  let rec load_lines acc =
    try
      let line = input_line ic in
      load_lines (split_by_comma line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  load_lines []

(* Authenticate a user by username and password *)
let authenticate username password users =
  List.find_opt (fun (u, p, _, _) -> u = username && p = password) users
