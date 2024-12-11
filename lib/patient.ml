include Account
open Csv

(** [display_prescription_statuses tasks_csv user] helps in find a user in the
    csv file of accounts. *)
let display_prescription_statuses tasks_csv user =
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
