[@@@warning "-a"]

(* =========================== DRIVER PROGRAM =========================== *)
(* Prescription Validator Program *)

(* Welcome message *)
let welcome_message () =
  print_endline "Welcome to the Prescription Validator!";
  print_endline "Choose an option:";
  print_endline "1. Login";
  print_endline "2. Signup"

(** Dummy Authenticator*)
let auth username pw = true

(** Dummy Signup*)
let signup username pw = ()

(* Function to authenticate a user *)
let login () =
  print_endline "Enter username:";
  let username = read_line () in
  print_endline "Enter password:";
  let password = read_line () in
  (* Assuming an auth function is available *)
  if auth username password then Some username else None

(* Function to register a new user *)
let signup () =
  print_endline "Create a username:";
  let username = read_line () in
  print_endline "Create a password:";
  let password = read_line () in
  (* Assuming a signup function is available *)
  signup username password;
  print_endline "Signup successful!";
  Some username

(* Placeholder function for patient's tasks *)
let show_patient () =
  print_endline "Here are your tasks as a patient:";
  print_endline "- Task 1: Take prescribed medication";
  print_endline "- Task 2: Schedule follow-up appointment"

(* Placeholder function for doctor's input *)
let doctor_form () =
  print_endline "Doctor, please input diagnosis and prescription:";
  print_endline "Enter diagnosis:";
  let diagnosis = read_line () in
  print_endline "Enter prescription:";
  let prescription = read_line () in
  print_endline "Diagnosis and prescription recorded."

(* Placeholder function for pharmacist tasks *)
let all_tasks () =
  print_endline "Here are the tasks for pharmacists:";
  print_endline "(1) Verify prescription:"
;;

(* Entrypoint *)
welcome_message ();
match read_int_opt () with
| Some 1 -> (
    (* Login flow *)
    match login () with
    | Some username -> (
        (* Placeholder roles for simplicity. Replace with actual role checks *)
        let role = "patient" in
        (* or "doctor" or "pharmacist" *)
        match role with
        | "patient" -> show_patient ()
        | "doctor" -> doctor_form ()
        | "pharmacist" -> all_tasks ()
        | _ -> print_endline "Invalid role.")
    | None -> print_endline "Login failed.")
| Some 2 -> (
    (* Signup flow *)
    match signup () with
    | Some username -> print_endline ("Welcome, " ^ username)
    | None -> print_endline "Signup failed.")
| _ -> print_endline "Invalid option. Please try again."
