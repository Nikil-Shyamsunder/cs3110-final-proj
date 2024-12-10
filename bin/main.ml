[@@@warning "-a"]

module Auth = Prescription_validator.Authenticator
module Account = Prescription_validator.Account
module Pharmacist = Prescription_validator.Pharmacist
module Doctor = Prescription_validator.Doctor
module Patient = Prescription_validator.Patient
module Task = Prescription_validator.Task

(* =========================== DRIVER PROGRAM =========================== *)
(* Prescription Validator Program *)

(* Set file paths *)
let accounts_path = "data/accounts.csv"
let tasks_path = "data/tasks.csv"
let tasks = Task.load_tasks_from_csv tasks_path

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
  let filename = accounts_path in
  let out_channel = open_out_gen [ Open_append; Open_creat ] 0o666 filename in
  (* Write the account information with the fourth column as an empty list *)
  Printf.fprintf out_channel "%s,%s,%s,\"[]\"\n" username pw role;
  close_out out_channel;
  Printf.printf
    "Successfully created %s. Quitting program, restart and login.\n" username

(* Function to authenticate a user *)
let login () =
  print_endline "Enter username:";
  let username = read_line () in
  print_endline "Enter password:";
  let password = read_line () in
  (* Assuming an auth function is available *)
  let usr_info = auth username password in
  match usr_info with
  | Some usr ->
      print_endline "Success!";
      usr
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

(* Placeholder function for patient loop *)
let patient_driver username pwd role =
  let usr = Patient.create_user username pwd role in
  Patient.display_prescription_statuses accounts_path tasks_path username

(* Placeholder function for doctor loop *)
let doctor_driver username pwd role =
  let usr = Doctor.create_user username pwd role in
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

  Doctor.add_diagnosis_prescription tasks_path accounts_path username patient
    diagnosis prescription

(* Placeholder function for pharmacist tasks *)
let pharmacist_driver username pwd role =
  let usr = Pharmacist.create_user username pwd role in
  Pharmacist.vote_on_task accounts_path tasks_path username
(* print_endline ("Pharmacist " ^ username ^ ", here are your tasks:");
   print_endline "(1) Verify Prescription: Amoxicillin for strep"; print_endline
   "(2) Verify prescription: Trimethoprim for UTI: " *)
;;

(* Entrypoint *)
welcome_message ();
match read_int_opt () with
| Some 1 -> (
    (* Login flow *)
    let u, p, r, lst = login () in
    match r with
    | "patient" -> patient_driver u p r
    | "doctor" -> doctor_driver u p r
    | "pharmacist" -> pharmacist_driver u p r
    | _ -> failwith "Invalid role")
| Some 2 ->
    (* Signup flow *)
    signup ()
| Some n -> failwith "Invalid number; try again. Exiting the program..."
| None -> failwith "Invalid input; try again. Exiting the program."
