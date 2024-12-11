include Account

(** [get_all_task_ids tasks_csv] gets the task IDs from tasks CSV *)
let get_all_task_ids (tasks_csv : Csv.t ref) =
  List.fold_left
    (fun acc row ->
      match row with
      | id :: _ -> ( try int_of_string id :: acc with _ -> acc)
      | _ -> acc)
    [] !tasks_csv

(** [find_task_row tasks_csv] finds a task row in the csv file of the available
    tasks by task ID *)
let find_task_row (tasks_csv : Csv.t ref) task_id =
  List.find_opt (fun row -> List.hd row = string_of_int task_id) !tasks_csv

(* Helper to update a task row with a vote *)
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

(* Helper to update a user's task list in accounts CSV *)
let update_user_tasks (accounts_csv : Csv.t ref) user task_id =
  accounts_csv :=
    List.map
      (fun row ->
        match row with
        | [ u; pw; role; task_list ] when u = username user ->
            let updated_task_list =
              task_id :: tasks user |> List.map string_of_int
              |> String.concat "," |> Printf.sprintf "[%s]"
            in
            [ u; pw; role; updated_task_list ]
        | _ -> row)
      !accounts_csv

(** [vote_on_task_core accounts_csv tasks_csv user task_id vote] records a vote
    on a task and updates the user's task list. *)
let vote_on_task_core (accounts_csv : Csv.t ref) (tasks_csv : Csv.t ref)
    (user : t) task_id vote =
  update_task_csv tasks_csv task_id (username user) vote;
  update_user_tasks accounts_csv user task_id;
  Printf.printf
    "Your vote has been recorded, and the task has been added to your tasks.\n"
