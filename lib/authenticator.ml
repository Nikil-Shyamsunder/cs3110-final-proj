open Csv

(* Load users from CSV file using the Csv module *)
let load_users filename =
  let ic = Csv.load filename in
  List.map
    (fun row ->
      match row with
      | [ username; password; role; data ] -> (username, password, role, data)
      | _ -> failwith "Malformed CSV line")
    ic

(* Authenticate a user by username and password *)
let authenticate username password users =
  List.find_opt (fun (u, p, _, _) -> u = username && p = password) users
