module VoteTally = struct
  type t = {
    voter_id : int list;
    total_votes : int;
  }
end

module Task = struct
  type t = {
    task_id : int;
    prescription : string;
    vote_count : VoteTally.t;
  }

  (** [parse_task row] parse a row from CSV into a Task.t *)
  let parse_task row =
    match row with
    | [ task_id; prescription; voter_ids; total_votes ] ->
        let voter_id_list =
          List.map int_of_string (String.split_on_char ',' voter_ids)
        in
        let vote_count =
          {
            VoteTally.voter_id = voter_id_list;
            total_votes = int_of_string total_votes;
          }
        in
        { task_id = int_of_string task_id; prescription; vote_count }
    | _ -> failwith "Incorrect CSV format for task"

  (** [load_tasks_from_csv file] to load tasks from a CSV file *)
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
