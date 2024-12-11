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

let role_to_string = function
  | Patient -> "patient"
  | Doctor -> "doctor"
  | Pharmacist -> "pharmacist"

let string_to_role = function
  | "patient" -> Patient
  | "doctor" -> Doctor
  | "pharmacist" -> Pharmacist
  | _ -> failwith "input invalid"

let create_user (u : string) (p : string) (role_op : string) (lst : int list) =
  { username = u; password = p; role = string_to_role role_op; task_list = lst }

let role acc = role_to_string acc.role
let username acc = acc.username
let tasks acc = acc.task_list

let find_user accounts_path username =
  let accounts_csv = Csv.load accounts_path in
  List.find_opt
    (fun row ->
      match row with
      | [ u; _; _; _ ] when u = username -> true
      | _ -> false)
    accounts_csv

(* Helper function to get a user's task list from the accounts CSV *)
let get_user_task_list accounts_path username =
  match find_user accounts_path username with
  | None -> None (* Return None if the user is not found *)
  | Some [ _; _; _; task_list_str ] ->
      (* Parse the task list from the 4th column *)
      let task_ids =
        String.sub
          (String.trim task_list_str)
          1
          (String.length task_list_str - 2)
        |> String.split_on_char ',' (* Split by commas *)
        |> List.filter (fun s -> String.trim s <> "") (* Remove empty strings *)
        |> List.map int_of_string (* Convert to integers *)
      in
      Some task_ids
  | Some _ -> failwith "Malformed accounts CSV: Expected 4 columns per row"

(* Helper function to update the final column of a user's row *)
let update_user_tasks (accounts_csv : Csv.t ref) (username : string) task_id =
  accounts_csv :=
    List.map
      (fun row ->
        match row with
        | [ u; pw; role; tasks ] when u = username ->
            let updated_tasks =
              (* Parse the existing list, add the task_id, and convert back to
                 string *)
              let tasks_list =
                try
                  String.sub (String.trim tasks) 1 (String.length tasks - 2)
                  (* Remove the square brackets *)
                  |> String.split_on_char ',' (* Split by comma *)
                  |> List.map String.trim
                  |> List.filter (fun x -> x <> "")
                  (* Remove empty strings *)
                with _ -> []
              in
              let new_tasks = string_of_int task_id :: tasks_list in
              Printf.sprintf "[%s]" (String.concat "," new_tasks)
            in
            [ u; pw; role; updated_tasks ]
        | _ -> row)
      !accounts_csv
