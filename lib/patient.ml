include Account
open Csv

(* Helper function to find a user in the accounts CSV *)
let display_prescription_statuses tasks_csv user =
  (* Check if the user's task list is empty *)
  match tasks user with
  | [] ->
      Printf.printf
        "Hello %s. There are no current prescriptions waiting for approval \
         relevant to you.\n"
        (username user)
  | task_ids ->
      Printf.printf
        "Welcome %s! Here is the status of your current prescriptions.\n"
        (username user);
      Task.display_tasks_from_ids !tasks_csv task_ids
