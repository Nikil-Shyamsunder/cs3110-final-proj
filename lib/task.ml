module VoteTally = struct
  type t = {
    yes_votes : int;
    yes_voters : string list;
    no_votes : int;
    no_voters : string list;
  }
end

module Task = struct
  type t = {
    task_id : int;
    prescription : string;
    vote_count : VoteTally.t;
  }

  (** Helper to parse voter IDs from a string *)
  let parse_voter_ids voter_ids_str =
    String.split_on_char ',' voter_ids_str |> List.map int_of_string

  (** [parse_task row] parses a row from CSV into a Task.t *)
  let parse_task row =
    match row with
    | task_id :: prescription :: total_votes :: voter_ids ->
        let voter_id_list = parse_voter_ids (String.concat "," voter_ids) in
        let vote_count =
          {
            VoteTally.voter_id = voter_id_list;
            total_votes = int_of_string total_votes;
          }
        in
        { task_id = int_of_string task_id; prescription; vote_count }
    | _ -> failwith "Incorrect CSV format for task"

  (** [load_tasks_from_csv filepath] loads tasks from a CSV file *)
  let load_tasks_from_csv filepath =
    let csv_data = Csv.load filepath in
    List.map parse_task csv_data

  (** [display_single_task task] to display a task *)
  let display_single_task task =
    Printf.printf
      "Task ID: %d\nPrescription: %s\nTotal Votes: %d\nVoter IDs: [%s]\n\n"
      task.task_id task.prescription task.vote_count.total_votes
      (String.concat ", " (List.map string_of_int task.vote_count.voter_id))

  (** [display_tasks filepath] displays all tasks from the CSV *)
  let display_tasks filepath =
    let tasks = load_tasks_from_csv filepath in
    List.iter display_single_task tasks
end
