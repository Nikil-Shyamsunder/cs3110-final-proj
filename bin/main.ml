[@@@warning "-a"]

open Prescription_validator.Accounts
module Auth = Prescription_validator.Accounts.Authenticator
module Account = Prescription_validator.Accounts.Account
module Pharmacist = Prescription_validator.Accounts.Pharmacist
module Doctor = Prescription_validator.Accounts.Doctor
module Patient = Prescription_validator.Accounts.Patient

(* =========================== DRIVER PROGRAM =========================== *)
(* Prescription Validator Program *)

(* Set file paths *)
let accounts_path = "data/accounts.csv"

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
  Printf.fprintf out_channel "%s,%s,%s\n" username pw role;
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
  | Some (u, p, r) ->
      print_endline "Success!";
      (u, p, r)
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
  print_endline
    ("Welcome patient " ^ username
   ^ ": Here are your outstanding prescriptions:");
  print_endline "- Prescription 1: Tylenol for headaches: Approved!";
  print_endline "- Prescription 2: Amoxicillin for strep: Pending approval..."

(* Placeholder function for doctor loop *)
let doctor_driver username pwd role =
  let usr = Doctor.create_user username pwd role in
  print_endline
    ("Doctor " ^ username ^ ", enter a new task to publish to blockchain:");
  print_endline "Enter diagnosis:";
  let diagnosis = read_line () in
  print_endline "Enter prescription:";
  let prescription = read_line () in
  print_endline "Diagnosis and prescription recorded."

(* Placeholder function for pharmacist tasks *)
let pharmacist_driver username pwd role =
  let usr = Pharmacist.create_user username pwd role in
  print_endline ("Pharmacist " ^ username ^ ", here are your tasks:");
  print_endline "(1) Verify Prescription: Amoxicillin for strep";
  print_endline "(2) Verify prescription: Trimethoprim for UTI: "
;;

(* Entrypoint *)
welcome_message ();
match read_int_opt () with
| Some 1 -> (
    (* Login flow *)
    let u, p, r = login () in
    match r with
    | "patient" -> patient_driver u p r
    | "doctor" -> doctor_driver u p r
    | "pharmacist" -> pharmacist_driver u p r
    | _ -> failwith "Invalid role")
| Some 2 ->
    (* Signup flow *)
    signup ()
