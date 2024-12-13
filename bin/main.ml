module Auth = Prescription_validator.Authenticator
module Account = Prescription_validator.Account
module Pharmacist = Prescription_validator.Pharmacist
module Doctor = Prescription_validator.Doctor
module Patient = Prescription_validator.Patient
module Task = Prescription_validator.Task
module Blockchain = Prescription_validator.Blockchain

(* =========================== DRIVER PROGRAM =========================== *)

(* Set file paths *)
let accounts_path = "data/accounts.csv"
let blockchain_path = "data/blockchain.json"
let accounts_csv = ref (Csv.load accounts_path)
let blockchain = Blockchain.load_blockchain_from_file blockchain_path

let () =
  if Blockchain.validate_blockchain blockchain then ()
  else failwith "You're blockchain is invalid. You cannot login. Exiting... "

let tasks_csv = ref (Blockchain.latest_tasks blockchain)

(* Welcome message *)
let welcome_message () =
  print_endline "Welcome to the Prescription Validator!";
  print_endline "Choose an option:";
  print_endline "1. Login";
  print_endline "2. Signup"

(* Authenticator*)
let auth username pw =
  Auth.authenticate username pw (Auth.load_users accounts_path)

(* Function to write an account to a CSV file *)
let create_account username pw role =
  (* Create a new row for the account information *)
  let new_row = [ username; pw; role; "[]" ] in

  (* Append the new row to the CSV reference *)
  accounts_csv := !accounts_csv @ [ new_row ]

(* Function to authenticate a user *)
let login () =
  print_endline "Enter username:";
  let username = read_line () in
  print_endline "Enter password:";
  let password = read_line () in
  (* Assuming an auth function is available *)
  let usr_info = auth username password in
  match usr_info with
  | Some (u, p, r, lst) ->
      print_endline "Success!";
      (u, p, r, Task.string_to_task_ids lst)
  | None -> failwith "Login Failed."

(* Function to register a new user *)
let signup () =
  print_endline "Create a username:";
  let username = read_line () in
  print_endline "Create a password:";
  let password = read_line () in
  print_endline "Enter your role ('patient, 'pharmacist', or 'doctor'):";
  let role = read_line () in
  (* Assuming a signup function is available *)
  create_account username password role

(* Function to print out a patient's relevant tasks *)
let patient_driver username pwd role lst =
  let usr = Patient.create_user username pwd role lst in
  let output = Patient.display_prescription_statuses tasks_csv usr in
  Printf.printf "%s" output

(* Function to ask a doctor to add a new task to the blockchain *)
let doctor_driver username pwd role lst =
  print_endline
    ("Doctor " ^ username ^ ", enter a new task to publish to blockchain:");
  print_endline
    "Who is the relevant patient (This information will NOT go on the \
     blockchain):";
  let patient = read_line () in
  print_endline "Enter diagnosis:";
  let diagnosis = read_line () in
  print_endline "Enter prescription:";
  let prescription = read_line () in
  Doctor.add_diagnosis_prescription tasks_csv accounts_csv username patient
    diagnosis prescription

(* Helper function for pharmacists to vote on tasks *)
let vote_on_task_driver accounts_csv_ref tasks_csv_ref user =
  (* Fetch all tasks *)
  let all_task_ids = Pharmacist.get_all_task_ids tasks_csv_ref in
  let user_task_ids = Pharmacist.tasks user in
  let votable_task_ids =
    List.filter (fun id -> not (List.mem id user_task_ids)) all_task_ids
  in

  (* Display votable tasks *)
  if votable_task_ids = [] then
    Printf.printf "There are no tasks available for voting.\n"
  else (
    Printf.printf "\nThese are the tasks you can vote on:\n";
    Printf.printf "%s"
      (Task.display_tasks_without_votes !tasks_csv_ref votable_task_ids);

    (* Ask for Task ID *)
    Printf.printf
      "\n\
       Enter the Task ID you would like to vote on (or type 'quit' to \
       abstain): ";
    let input = read_line () in
    if String.trim input = "quit" then
      Printf.printf "You chose to abstain from voting. Goodbye!\n"
    else
      try
        let task_id = int_of_string input in
        match Pharmacist.find_task_row tasks_csv_ref task_id with
        | None -> Printf.printf "Invalid Task ID. Exiting.\n"
        | Some _ ->
            Printf.printf "\nYou selected Task ID %d.\n" task_id;

            (* Prompt for vote *)
            Printf.printf "\nHow would you like to vote? (yes/no): ";
            let vote = read_line () in

            if vote = "yes" || vote = "no" then (
              (* Process the vote *)
              Pharmacist.vote_on_task_core accounts_csv_ref tasks_csv_ref user
                task_id vote;
              Printf.printf
                "Your vote has been recorded, and the task has been added to \
                 your tasks.\n";
              Printf.printf "Thank you for voting. Goodbye!\n")
            else Printf.printf "Invalid vote. Exiting without voting.\n"
      with Failure _ ->
        Printf.printf "Invalid input. Exiting without voting.\n")

(* Driver function for pharmacists to vote on tasks *)
let pharmacist_driver username pwd role lst =
  let usr = Pharmacist.create_user username pwd role lst in
  vote_on_task_driver accounts_csv tasks_csv usr
;;

(* Entrypoint *)
welcome_message ();
match read_int_opt () with
| Some 1 -> (
    (* Login flow *)
    let u, p, r, lst = login () in
    match r with
    | "patient" -> patient_driver u p r lst
    | "doctor" -> doctor_driver u p r lst
    | "pharmacist" -> pharmacist_driver u p r lst
    | _ -> failwith "Invalid role")
| Some 2 ->
    (* Signup flow *)
    signup ()
| Some n -> failwith "Invalid number; try again. Exiting the program..."
| None -> failwith "Invalid input; try again. Exiting the program."
;;

Csv.save accounts_path !accounts_csv;
let new_block = Blockchain.create_block blockchain !tasks_csv 2 in
let blockchain = Blockchain.append_block blockchain new_block in
Blockchain.save_blockchain_to_file blockchain "data/blockchain.json"
