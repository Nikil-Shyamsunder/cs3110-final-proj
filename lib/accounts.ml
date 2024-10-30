(* authenticator.ml *)
module Authenticator : sig
  val load_users : string -> (string * string * string) list

  val authenticate :
    string ->
    string ->
    (string * string * string) list ->
    (string * string * string) option
end = struct
  (* Helper function to split a string by commas *)
  let split_by_comma line =
    let parts = String.split_on_char ',' line in
    match parts with
    | [ username; password; role ] -> (username, password, role)
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
    List.find_opt (fun (u, p, r) -> u = username && p = password) users
end

module Account = struct
  type role =
    | Doctor
    | Pharmacist
    | Patient

  type t = {
    role : role;
    username : string;
    password : string;
    task_list : int list;
  }

  let role_display = function
    | Doctor -> "Doctor"
    | Pharmacist -> "Pharmacist"
    | Patient -> "Patient"

  let role_to_string = function
    | Patient -> "patient"
    | Doctor -> "doctor"
    | Pharmacist -> "pharmacist"

  let string_to_role = function
    | "patient" -> Patient
    | "doctor" -> Doctor
    | "pharmacist" -> Pharmacist
    | _ -> failwith "input invalid"

  let create_user (u : string) (p : string) (role_op : string) =
    {
      username = u;
      password = p;
      role = string_to_role role_op;
      task_list = [];
    }

  let role acc = acc.role
end

module Pharmacist = struct
  include Account
end

module Patient = struct
  include Account
end

module Doctor = struct
  include Account
end
