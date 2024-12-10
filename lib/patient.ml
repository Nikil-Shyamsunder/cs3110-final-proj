include Account
open Csv

(* Helper function to find a user in the accounts CSV *)
let display_prescription_statuses accounts_path tasks_path username =
  match get_user_task_list accounts_path username with
  | None -> Printf.printf "User %s not found in accounts.\n" username
  | Some [] ->
      Printf.printf
        "Hello %s. There are no current prescriptions waiting for approval \
         relevant to you.\n"
        username
  | Some task_ids ->
      Printf.printf
        "Welcome %s! Here is the status of your current prescriptions.\n"
        username;
      Task.display_tasks_from_ids tasks_path task_ids
