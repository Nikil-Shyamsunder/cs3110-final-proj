include Account

let get_all_task_ids tasks_csv =
  List.fold_left
    (fun acc row ->
      match row with
      | id :: _ -> ( try int_of_string id :: acc with _ -> acc)
      | _ -> acc)
    [] tasks_csv

let find_task_row tasks_csv task_id =
  List.find_opt (fun row -> List.hd row = string_of_int task_id) tasks_csv

let update_task_csv (tasks_csv : Csv.t) task_id username vote =
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
    tasks_csv

let update_user_tasks (accounts_csv : Csv.t) (user : t) task_id =
  List.map
    (fun row ->
      match row with
      | [ u; pw; role; task_list ] when u = username user ->
          let updated_task_list =
            task_id :: tasks user |> List.map string_of_int |> String.concat ","
            |> Printf.sprintf "[%s]"
          in
          [ u; pw; role; updated_task_list ]
      | _ -> row)
    accounts_csv

let vote_on_task (accounts_csv : Csv.t ref) (tasks_csv : Csv.t ref) (user : t) =
  let user_task_ids = tasks user in
  let all_task_ids = get_all_task_ids !tasks_csv in
  let votable_task_ids =
    List.filter (fun id -> not (List.mem id user_task_ids)) all_task_ids
  in

  if votable_task_ids = [] then
    Printf.printf "There are no tasks available for voting.\n"
  else (
    Printf.printf "\nThese are the tasks you can vote on:\n";
    Task.display_tasks_from_ids !tasks_csv votable_task_ids;

    (* Ask the user to select a task to vote on *)
    Printf.printf "\nEnter the Task ID you would like to vote on: ";
    let task_id = read_int () in

    match find_task_row !tasks_csv task_id with
    | None -> Printf.printf "Invalid Task ID.\n"
    | Some task_row ->
        Printf.printf "\nYou selected:\n";
        Task.display_tasks_from_ids !tasks_csv [ task_id ];

        (* Ask for vote *)
        Printf.printf "\nHow would you like to vote? (yes/no): ";
        let vote = read_line () in
        if vote = "yes" || vote = "no" then (
          (* Update tasks CSV *)
          tasks_csv := update_task_csv !tasks_csv task_id (username user) vote;

          (* Update accounts CSV *)
          accounts_csv := update_user_tasks !accounts_csv user task_id;

          Printf.printf
            "Your vote has been recorded, and the task has been added to your \
             tasks.\n")
        else Printf.printf "Invalid vote. No changes made.\n")
