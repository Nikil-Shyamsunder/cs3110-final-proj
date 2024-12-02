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
    diagnosis : string;
    prescription : string;
    vote_count : VoteTally.t;
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
            VoteTally.yes_votes = int_of_string yes_votes;
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

  (** [display_single_task task] to display a task *)
  let display_single_task task =
    Printf.printf
      "Task ID: %d\n\
       Diagnosis: %s\n\
       Prescription: %s\n\
       Yes Votes: %d\n\
       Yes Voters: [%s]\n\
       No Votes: %d\n\
       No Voters: [%s]\n\n"
      task.task_id task.diagnosis task.prescription task.vote_count.yes_votes
      (String.concat ", " task.vote_count.yes_voters)
      task.vote_count.no_votes
      (String.concat ", " task.vote_count.no_voters)

  (** [display_tasks filepath] displays all tasks from the CSV *)
  let display_tasks filepath =
    let tasks = load_tasks_from_csv filepath in
    List.iter display_single_task tasks
end
