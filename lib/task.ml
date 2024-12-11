type voteTally = {
  yes_votes : int;
  yes_voters : string list;
  no_votes : int;
  no_voters : string list;
}

type t = {
  task_id : int;
  diagnosis : string;
  prescription : string;
  vote_count : voteTally;
}

(** Helper to parse voter IDs from a string *)
let parse_voter_ids voter_ids_str =
  String.split_on_char ',' voter_ids_str |> List.map String.trim

(** [parse_task row] parses a row from CSV into a Task.t *)
let parse_task row =
  match row with
  | [
   task_id;
   diagnosis;
   prescription;
   yes_votes;
   yes_voters_str;
   no_votes;
   no_voters_str;
  ] ->
      let yes_voters = parse_voter_ids yes_voters_str in
      let no_voters = parse_voter_ids no_voters_str in
      let vote_count =
        {
          yes_votes = int_of_string yes_votes;
          yes_voters;
          no_votes = int_of_string no_votes;
          no_voters;
        }
      in
      { task_id = int_of_string task_id; diagnosis; prescription; vote_count }
  | _ -> failwith "Incorrect CSV format for task"

(** [load_tasks_from_csv filepath] loads tasks from a CSV file *)
let load_tasks_from_csv filepath =
  let csv_data = Csv.load filepath in
  List.map parse_task csv_data

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

  if task_ids = [] then Printf.printf "There are no tasks to display.\n"
  else (
    (* Print a header with fixed-width columns, excluding vote counts *)
    Printf.printf "%-8s | %-15s | %-15s\n" "Task ID" "Diagnosis" "Prescription";
    Printf.printf "-----------------------------------------------------\n";

    (* For each task ID, find and print the corresponding task *)
    List.iter
      (fun task_id ->
        match find_task tasks_csv task_id with
        | None -> Printf.printf "Task with ID %d not found in tasks.\n" task_id
        | Some task_row ->
            (* Extract the desired columns: Task ID, Diagnosis, Prescription *)
            let task_id = List.nth task_row 0 in
            let diagnosis = List.nth task_row 1 in
            let prescription = List.nth task_row 2 in
            Printf.printf "%-8s | %-15s | %-15s\n" task_id diagnosis
              prescription)
      task_ids)

let string_to_task_ids task_list_str =
  String.sub (String.trim task_list_str) 1 (String.length task_list_str - 2)
  |> String.split_on_char ',' (* Split by commas *)
  |> List.filter (fun s -> String.trim s <> "") (* Remove empty strings *)
  |> List.map int_of_string (* Convert to integers *)
