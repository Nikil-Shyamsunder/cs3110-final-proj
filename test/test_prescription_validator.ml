open OUnit2
open Csv
open Prescription_validator.Account
open Prescription_validator.Authenticator
open Prescription_validator.Doctor
open Prescription_validator.Patient
open Prescription_validator.Pharmacist
open Prescription_validator.Task
open Prescription_validator

let create_doctor _ =
  let doctor = Doctor.create_user "Dr. xyz" "password123" "doctor" [] in
  assert_equal "doctor" (Doctor.role doctor);
  assert_equal "Dr. xyz" (Doctor.username doctor);
  assert_equal [] (Doctor.tasks doctor)

let create_patient _ =
  let patient = Patient.create_user "abc" "password123" "patient" [] in
  assert_equal "patient" (Patient.role patient);
  assert_equal "abc" (Patient.username patient);
  assert_equal [] (Patient.tasks patient)

let create_pharmacist _ =
  let pharmacist =
    Pharmacist.create_user "pharm" "password123" "pharmacist" []
  in
  assert_equal "pharmacist" (Pharmacist.role pharmacist);
  assert_equal "pharm" (Pharmacist.username pharmacist);
  assert_equal [] (Pharmacist.tasks pharmacist)

let test_find_user _ =
  (* Create a temporary CSV file *)
  let accounts_file = "temp_accounts.csv" in
  let accounts_csv =
    [
      [ "doctor1"; "password"; "doctor"; "[1,2]" ];
      [ "patient1"; "password"; "patient"; "[3,4]" ];
    ]
  in
  Csv.save accounts_file accounts_csv;

  (* Test find_user function *)
  let result = Account.find_user accounts_file "patient1" in
  let expected = Some [ "patient1"; "password"; "patient"; "[3,4]" ] in

  (* Assert result *)
  assert_equal expected result ~msg:"Failed to find correct user.";

  (* Clean up the temporary file *)
  Sys.remove accounts_file

let test_get_user_task_list _ =
  (* Create a temporary accounts CSV file *)
  let temp_file = Filename.temp_file "accounts" ".csv" in
  let csv_data =
    [
      [ "alice"; "password1"; "doctor"; "[1,2,3]" ];
      [ "bob"; "password2"; "patient"; "[4,5]" ];
      [ "charlie"; "password3"; "pharmacist"; "[]" ];
    ]
  in
  Csv.save temp_file csv_data;

  (* Test get_user_task_list *)
  let bob_tasks = get_user_task_list temp_file "bob" in
  let alice_tasks = get_user_task_list temp_file "alice" in
  let non_existent_user = get_user_task_list temp_file "dave" in

  assert_equal (Some [ 4; 5 ]) bob_tasks ~msg:"Failed to get tasks for Bob.";
  assert_equal
    (Some [ 1; 2; 3 ])
    alice_tasks ~msg:"Failed to get tasks for Alice.";
  assert_equal None non_existent_user
    ~msg:"Non-existent user should return None.";

  (* Clean up the temporary file *)
  Sys.remove temp_file

let test_add_diagnosis_prescription _ =
  let tasks_csv_ref =
    ref
      [
        [ "1"; "Diagnosis1"; "Prescription1"; "0"; "[]"; "0"; "[]" ];
        [ "2"; "Diagnosis2"; "Prescription2"; "0"; "[]"; "0"; "[]" ];
      ]
  in
  let accounts_csv_ref =
    ref
      [
        [ "doctor1"; "password"; "doctor"; "[]" ];
        [ "patient1"; "password"; "patient"; "[1,2]" ];
      ]
  in
  add_diagnosis_prescription tasks_csv_ref accounts_csv_ref "doctor1" "patient1"
    "Diagnosis3" "Prescription3";
  let expected_tasks_csv =
    [
      [ "1"; "Diagnosis1"; "Prescription1"; "0"; "[]"; "0"; "[]" ];
      [ "2"; "Diagnosis2"; "Prescription2"; "0"; "[]"; "0"; "[]" ];
      [ "3"; "Diagnosis3"; "Prescription3"; "0"; "[]"; "0"; "[]" ];
    ]
  in
  assert_equal expected_tasks_csv !tasks_csv_ref
    ~msg:"tasks CSV not updated correctly";
  let expected_accounts_csv =
    [
      [ "doctor1"; "password"; "doctor"; "[3]" ];
      [ "patient1"; "password"; "patient"; "[3,1,2]" ];
    ]
  in
  assert_equal expected_accounts_csv !accounts_csv_ref
    ~msg:"accounts\n\n   CSV not updated correctly"

let test_get_all_task_ids _ =
  let temp_file = Filename.temp_file "tasks" ".csv" in
  let csv_data =
    [
      [ "1"; "Diagnosis1"; "Prescription1"; "0"; "[]"; "0"; "[]" ];
      [ "2"; "Diagnosis2"; "Prescription2"; "0"; "[]"; "0"; "[]" ];
      [ "3"; "Diagnosis3"; "Prescription3"; "0"; "[]"; "0"; "[]" ];
    ]
  in
  Csv.save temp_file csv_data;
  let tasks_csv_ref = ref (Csv.load temp_file) in
  let task_ids = get_all_task_ids tasks_csv_ref in
  assert_equal [ 3; 2; 1 ] task_ids ~msg:"Failed to get all task ids correctly.";
  Sys.remove temp_file

let test_find_task_row _ =
  let temp_file = Filename.temp_file "tasks" ".csv" in
  let csv_data =
    [
      [ "1"; "Diagnosis1"; "Prescription1"; "0"; "[]"; "0"; "[]" ];
      [ "2"; "Diagnosis2"; "Prescription2"; "0"; "[]"; "0"; "[]" ];
      [ "3"; "Diagnosis3"; "Prescription3"; "0"; "[]"; "0"; "[]" ];
    ]
  in
  Csv.save temp_file csv_data;
  let tasks_csv_ref = ref (Csv.load temp_file) in
  let task_row = find_task_row tasks_csv_ref 2 in
  assert_equal
    (Some [ "2"; "Diagnosis2"; "Prescription2"; "0"; "[]"; "0"; "[]" ])
    task_row ~msg:"Failed to find the correct task row.";
  let non_existent_task_row = find_task_row tasks_csv_ref 4 in
  assert_equal None non_existent_task_row
    ~msg:"Non-existent task should not be found.";
  Sys.remove temp_file

let suite =
  "test suite"
  >::: [
         "create_doctor" >:: create_doctor;
         "create_patient" >:: create_patient;
         "create_pharmacist" >:: create_pharmacist;
         "test_find_user" >:: test_find_user;
         "test_get_user_task_list" >:: test_get_user_task_list;
         "test_add_diagnosis_prescription" >:: test_add_diagnosis_prescription;
         "test_get_all_task_ids" >:: test_get_all_task_ids;
         "test_find_task_row" >:: test_find_task_row;
       ]

let () = run_test_tt_main suite
