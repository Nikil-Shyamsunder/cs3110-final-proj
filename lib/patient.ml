include Account
open Csv

(** [display_prescription_statuses tasks_csv user] helps in find a user in the
    csv file of accounts. *)
let display_prescription_statuses tasks_csv user =
  (* Check if the user's task list is empty and return a string *)

  match tasks user with
  | [] ->
      Printf.sprintf
        "Hello %s. There are no current prescriptions waiting for approval \
         relevant to you.\n"
        (username user)
  | task_ids ->
      let header =
        Printf.sprintf
          "Welcome %s! Here is the status of your current prescriptions.\n"
          (username user)
      in
      let task_details = Task.display_tasks_from_ids !tasks_csv task_ids in
      header ^ task_details
