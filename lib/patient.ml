include Account
open Csv

let display_prescription_statuses accounts_path tasks_path username =
  (* Helper function to find a user in the accounts CSV *)
  let find_user accounts_csv username =
    List.find_opt
      (fun row ->
        match row with
        | [ u; _; _; _ ] when u = username -> true
        | _ -> false)
      accounts_csv
  in

  (* Helper function to parse task list from the 4th column *)
  let parse_task_list tasks =
    String.sub (String.trim tasks) 1 (String.length tasks - 2)
    |> String.split_on_char ',' (* Split by commas *)
    |> List.filter (fun s -> String.trim s <> "") (* Remove empty strings *)
    |> List.map int_of_string (* Convert to integers *)
  in

  (* Helper function to find a task by task ID in the tasks CSV *)
  let find_task tasks_csv task_id =
    List.find_opt
      (fun row ->
        match row with
        | id :: _ when int_of_string id = task_id -> true
        | _ -> false)
      tasks_csv
  in

  (* Load accounts and tasks CSVs *)
  let accounts_csv = Csv.load accounts_path in
  let tasks_csv = Csv.load tasks_path in

  (* Find the user and get their task list *)
  match find_user accounts_csv username with
  | None -> Printf.printf "User %s not found in accounts.\n" username
  | Some [ _; _; _; task_list_str ] ->
      let task_ids = parse_task_list task_list_str in
      if task_ids = [] then
        Printf.printf
          "There are no current prescriptions waiting for approval relevant to \
           you.\n"
      else (
        (* Print a header with fixed-width columns *)
        Printf.printf "%-8s | %-15s | %-15s | %-8s | %-8s\n" "Task ID"
          "Diagnosis" "Prescription" "Yes Vote" "No Vote";
        Printf.printf
          "--------------------------------------------------------------\n";
        (* For each task ID, find and print the corresponding task *)
        List.iter
          (fun task_id ->
            match find_task tasks_csv task_id with
            | None ->
                Printf.printf "Task with ID %d not found in tasks.\n" task_id
            | Some task_row ->
                (* Extract the desired columns: Task ID, Diagnosis,
                   Prescription, Yes Vote, No Vote *)
                let task_id = List.nth task_row 0 in
                let diagnosis = List.nth task_row 1 in
                let prescription = List.nth task_row 2 in
                let yes_vote = List.nth task_row 3 in
                let no_vote = List.nth task_row 5 in
                Printf.printf "%-8s | %-15s | %-15s | %-8s | %-8s\n" task_id
                  diagnosis prescription yes_vote no_vote)
          task_ids)
  | Some _ -> failwith "Malformed accounts CSV: Expected 4 columns per row"
