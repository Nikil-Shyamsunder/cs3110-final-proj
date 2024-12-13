include Account

(* AF: The Pharmacist module represents a pharmacist account in the system. It
   includes all the functionalities of an Account, and provides additional
   functionalities specific to pharmacists, such as retrieving all task IDs from
   a CSV file and finding a specific task row by task ID. *)

(* RI: - The tasks_csv reference point to a valid CSV structure where each row
   in the tasks_csv should have a valid task ID as the first element and the
   task ID should be a non-negative integer. The tasks_csv should not contain
   any duplicate task IDs. The tasks_csv should not contain any malformed rows
   (i.e., rows with missing or extra columns). *)
let get_all_task_ids (tasks_csv : Csv.t ref) =
  List.fold_left
    (fun acc row ->
      match row with
      | id :: _ -> ( try int_of_string id :: acc with _ -> acc)
      | _ -> acc)
    [] !tasks_csv

let find_task_row (tasks_csv : Csv.t ref) task_id =
  List.find_opt (fun row -> List.hd row = string_of_int task_id) !tasks_csv

(** [update_task_csv tasks_csv task_id username vote] is a helper function that
    updates the [tasks_csv] whenver a user votes on a task_id with yes or no
    vote. *)
let update_task_csv (tasks_csv : Csv.t ref) task_id username vote =
  tasks_csv :=
    List.map
      (fun row ->
        if List.hd row = string_of_int task_id then
          let yes_vote = int_of_string (List.nth row 3) in
          let no_vote = int_of_string (List.nth row 5) in
          let yes_usernames = List.nth row 4 |> String.trim in
          let no_usernames = List.nth row 6 |> String.trim in
          match vote with
          | "yes" ->
              let updated_yes_usernames =
                String.sub yes_usernames 1 (String.length yes_usernames - 2)
                |> String.split_on_char ','
                |> List.filter (fun x -> x <> "")
                |> fun users ->
                username :: users |> List.map String.trim |> String.concat ","
                |> Printf.sprintf "[%s]"
              in
              List.mapi
                (fun i col ->
                  match i with
                  | 3 -> string_of_int (yes_vote + 1)
                  | 4 -> updated_yes_usernames
                  | _ -> col)
                row
          | "no" ->
              let updated_no_usernames =
                String.sub no_usernames 1 (String.length no_usernames - 2)
                |> String.split_on_char ','
                |> List.filter (fun x -> x <> "")
                |> fun users ->
                username :: users |> List.map String.trim |> String.concat ","
                |> Printf.sprintf "[%s]"
              in
              List.mapi
                (fun i col ->
                  match i with
                  | 5 -> string_of_int (no_vote + 1)
                  | 6 -> updated_no_usernames
                  | _ -> col)
                row
          | _ -> row
        else row)
      !tasks_csv

let vote_on_task_core (accounts_csv : Csv.t ref) (tasks_csv : Csv.t ref)
    (user : t) task_id vote =
  update_task_csv tasks_csv task_id (username user) vote;
  update_user_tasks accounts_csv (username user) task_id
