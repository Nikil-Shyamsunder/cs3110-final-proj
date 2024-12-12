type t = Csv.t

let display_tasks_from_ids (tasks_csv : Csv.t) task_ids =
  (* Helper function to find a task by task ID in the tasks CSV *)
  let find_task tasks_csv task_id =
    List.find_opt
      (fun row ->
        match row with
        | id :: _ when int_of_string id = task_id -> true
        | _ -> false)
      tasks_csv
  in

  if task_ids = [] then "There are no tasks to display.\n"
  else
    let header =
      Printf.sprintf "%-8s | %-15s | %-15s | %-8s | %-8s\n" "Task ID"
        "Diagnosis" "Prescription" "Yes Vote" "No Vote"
      ^ Printf.sprintf
          "--------------------------------------------------------------\n"
    in

    let task_rows =
      List.fold_left
        (fun acc task_id ->
          match find_task tasks_csv task_id with
          | None ->
              acc
              ^ Printf.sprintf "Task with ID %d not found in tasks.\n" task_id
          | Some task_row ->
              (* Extract the desired columns: Task ID, Diagnosis, Prescription,
                 Yes Vote, No Vote *)
              let task_id = List.nth task_row 0 in
              let diagnosis = List.nth task_row 1 in
              let prescription = List.nth task_row 2 in
              let yes_vote = List.nth task_row 3 in
              let no_vote = List.nth task_row 5 in
              acc
              ^ Printf.sprintf "%-8s | %-15s | %-15s | %-8s | %-8s\n" task_id
                  diagnosis prescription yes_vote no_vote)
        "" task_ids
    in
    header ^ task_rows

let display_tasks_without_votes (tasks_csv : Csv.t) task_ids =
  (* Helper function to find a task by task ID in the tasks CSV *)
  let find_task tasks_csv task_id =
    List.find_opt
      (fun row ->
        match row with
        | id :: _ when int_of_string id = task_id -> true
        | _ -> false)
      tasks_csv
  in

  if task_ids = [] then "There are no tasks to display.\n"
  else
    (* Initialize the result string with the header *)
    let result =
      Printf.sprintf "%-8s | %-15s | %-15s\n" "Task ID" "Diagnosis"
        "Prescription"
      ^ Printf.sprintf "-----------------------------------------------------\n"
    in

    (* For each task ID, find and format the corresponding task *)
    let tasks_string =
      List.fold_left
        (fun acc task_id ->
          match find_task tasks_csv task_id with
          | None ->
              acc
              ^ Printf.sprintf "Task with ID %d not found in tasks.\n" task_id
          | Some task_row ->
              (* Extract the desired columns: Task ID, Diagnosis,
                 Prescription *)
              let task_id = List.nth task_row 0 in
              let diagnosis = List.nth task_row 1 in
              let prescription = List.nth task_row 2 in
              acc
              ^ Printf.sprintf "%-8s | %-15s | %-15s\n" task_id diagnosis
                  prescription)
        result task_ids
    in

    tasks_string

let string_to_task_ids task_list_str =
  let trimmed_str = String.trim task_list_str in
  (* Ensure the string starts with '[' and ends with ']' *)
  if
    String.length trimmed_str < 2
    || trimmed_str.[0] <> '['
    || trimmed_str.[String.length trimmed_str - 1] <> ']'
  then invalid_arg "Input string must be enclosed in square brackets"
  else
    let content = String.sub trimmed_str 1 (String.length trimmed_str - 2) in
    (* Split by commas and process *)
    content |> String.split_on_char ',' |> List.map String.trim
    |> List.filter (fun s -> s <> "")
    |> List.map int_of_string
