open OUnit2
open Yojson.Basic.Util
open Csv
open Prescription_validator.Account
open Prescription_validator.Authenticator
open Prescription_validator.Doctor
open Prescription_validator.Patient
open Prescription_validator.Pharmacist
open Prescription_validator.Task
open Prescription_validator.Block
open Prescription_validator.Blockchain
open Prescription_validator

(* ======================= TEST ACCOUNT ======================= *)

(** [test_create_doctor] creates a doctor and checks the account to ensure that
    it is a valid doctor with proper username and with the role doctor. *)
let test_create_doctor _ =
  let doctor = Doctor.create_user "Dr. xyz" "password123" "doctor" [] in
  assert_equal "doctor" (Doctor.role doctor);
  assert_equal "Dr. xyz" (Doctor.username doctor);
  assert_equal [] (Doctor.tasks doctor)

(** [test_create_patient] creates a patient and checks the account to ensure
    that it is a valid patient with proper username and with the role doctor. *)
let test_create_patient _ =
  let patient = Patient.create_user "abc" "password123" "patient" [] in
  assert_equal "patient" (Patient.role patient);
  assert_equal "abc" (Patient.username patient);
  assert_equal [] (Patient.tasks patient)

(** [test_create_pharmacist] creates a pharmacist and checks the account to
    ensure that it is a valid pharmacist with proper username and with the role
    doctor. *)
let test_create_pharmacist _ =
  let pharmacist =
    Pharmacist.create_user "pharm" "password123" "pharmacist" []
  in
  assert_equal "pharmacist" (Pharmacist.role pharmacist);
  assert_equal "pharm" (Pharmacist.username pharmacist);
  assert_equal [] (Pharmacist.tasks pharmacist)

(** [test_create_invalid] creates an invalid user to check if our functions are
    properly raising the errors. *)
let test_create_invalid _ =
  assert_raises (Stdlib.Failure "input invalid") (fun () ->
      Account.create_user "invalid" "invalid" "invalid" []);
  assert_raises (Stdlib.Failure "input invalid") (fun () ->
      Patient.create_user "invalid" "invalid" "invalid" []);
  assert_raises (Stdlib.Failure "input invalid") (fun () ->
      Doctor.create_user "invalid" "invalid" "invalid" []);
  assert_raises (Stdlib.Failure "input invalid") (fun () ->
      Pharmacist.create_user "invalid" "invalid" "invalid" [])

(** [test_find_user] finds a user from the account csv when the user provides
    their details.*)
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

  let result = Account.find_user accounts_file "patient1" in
  let expected = Some [ "patient1"; "password"; "patient"; "[3,4]" ] in

  assert_equal expected result ~msg:"Failed to find correct user.";
  Sys.remove accounts_file

(** [test_get_user_task_list] creates a temporary CSV file with testing data,
    and then calls the function to get the task list for a specific user, and
    then verifies that the returned task list matches the expected task list. *)
let test_get_user_task_list _ =
  (* Helper function to create a temporary CSV file *)
  let create_temp_csv data =
    let temp_file = Filename.temp_file "accounts" ".csv" in
    Csv.save temp_file data;
    temp_file
  in

  let normal_csv_data =
    [
      [ "alice"; "password1"; "doctor"; "[1,2,3]" ];
      [ "bob"; "password2"; "patient"; "[4,5]" ];
      [ "charlie"; "password3"; "pharmacist"; "[]" ];
    ]
  in
  let temp_file_normal = create_temp_csv normal_csv_data in
  let bob_tasks = get_user_task_list temp_file_normal "bob" in
  let alice_tasks = get_user_task_list temp_file_normal "alice" in
  let non_existent_user = get_user_task_list temp_file_normal "dave" in

  assert_equal (Some [ 4; 5 ]) bob_tasks ~msg:"Failed to get tasks for Bob.";
  assert_equal
    (Some [ 1; 2; 3 ])
    alice_tasks ~msg:"Failed to get tasks for Alice.";
  assert_equal None non_existent_user
    ~msg:"Non-existent user should return None.";

  Sys.remove temp_file_normal

(* ======================= TEST DOCTOR ======================= *)

(** [test_add_diagnosis_prescription _] creates temporary CSV references for
    tasks and accounts, adds a diagnosis and prescription, and verifies that the
    tasks and accounts CSVs are updated correctly. *)
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

(* ======================= TEST PHARMACIST ======================= *)

(** [test_get_all_task_ids _] creates a temporary CSV file with testing data,
    calls the function to get all task IDs, and verifies that the returned task
    IDs match the expected task IDs. *)
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

(** [test_find_task_row _] creates a temporary CSV file with testing data, calls
    the function to find a task row by task ID, and verifies that the returned
    task row matches the expected task row. *)
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

(** [test_vote_on_task_core _] creates temporary CSV references for tasks and
    accounts, records a vote on a task, updates the user's task list, and
    verifies that the tasks and accounts CSVs are updated correctly. *)
let test_vote_on_task_core _ =
  let temp_file_tasks = Filename.temp_file "tasks" ".csv" in
  let temp_file_accounts = Filename.temp_file "accounts" ".csv" in

  let tasks_csv_data =
    [
      [ "1"; "Diagnosis1"; "Prescription1"; "0"; "[]"; "0"; "[]" ];
      [ "2"; "Diagnosis2"; "Prescription2"; "0"; "[]"; "0"; "[]" ];
    ]
  in
  let accounts_csv_data =
    [
      [ "alice"; "password1"; "pharmacist"; "[]" ];
      [ "alice2"; "password2"; "pharmacist"; "[]" ];
      [ "alice3"; "password3"; "pharmacist"; "" ];
      [ "bob"; "password2"; "patient"; "[3,4]" ];
    ]
  in

  Csv.save temp_file_tasks tasks_csv_data;
  Csv.save temp_file_accounts accounts_csv_data;

  let tasks_csv = ref (Csv.load temp_file_tasks) in
  let accounts_csv = ref (Csv.load temp_file_accounts) in

  let user = Pharmacist.create_user "alice" "password1" "pharmacist" [] in
  let user2 = Pharmacist.create_user "alice2" "password2" "pharmacist" [] in
  let user3 = Pharmacist.create_user "alice3" "password3" "pharmacist" [] in

  (* Test voting "yes" works properly *)
  vote_on_task_core accounts_csv tasks_csv user 1 "yes";

  let updated_task_row =
    List.hd (List.filter (fun row -> List.hd row = "1") !tasks_csv)
  in
  assert_equal
    [ "1"; "Diagnosis1"; "Prescription1"; "1"; "[alice]"; "0"; "[]" ]
    updated_task_row;

  let updated_user_row =
    List.hd (List.filter (fun row -> List.hd row = "alice") !accounts_csv)
  in
  assert_equal [ "alice"; "password1"; "pharmacist"; "[1]" ] updated_user_row;

  (* Test voting "no" works properly *)
  vote_on_task_core accounts_csv tasks_csv user2 1 "no";

  let updated_task_row_no =
    List.hd (List.filter (fun row -> List.hd row = "1") !tasks_csv)
  in
  assert_equal
    [ "1"; "Diagnosis1"; "Prescription1"; "1"; "[alice]"; "1"; "[alice2]" ]
    updated_task_row_no;

  let updated_user_row_no =
    List.hd (List.filter (fun row -> List.hd row = "alice2") !accounts_csv)
  in
  assert_equal
    [ "alice2"; "password2"; "pharmacist"; "[1]" ]
    updated_user_row_no;

  (* Ensure list is initialized properly on a user with "" in that column
     initially *)
  vote_on_task_core accounts_csv tasks_csv user3 2 "no";

  let updated_task_row_no =
    List.hd (List.filter (fun row -> List.hd row = "2") !tasks_csv)
  in
  assert_equal
    [ "2"; "Diagnosis2"; "Prescription2"; "0"; "[]"; "1"; "[alice3]" ]
    updated_task_row_no;

  let updated_user_row_no =
    List.hd (List.filter (fun row -> List.hd row = "alice3") !accounts_csv)
  in
  assert_equal
    [ "alice3"; "password3"; "pharmacist"; "[2]" ]
    updated_user_row_no;

  (* Ensure that two votes the same way on the same task are rendered correctly
     with commas. *)
  vote_on_task_core accounts_csv tasks_csv user2 2 "no";

  let updated_task_row_no =
    List.hd (List.filter (fun row -> List.hd row = "2") !tasks_csv)
  in
  assert_equal
    [ "2"; "Diagnosis2"; "Prescription2"; "0"; "[]"; "2"; "[alice2,alice3]" ]
    updated_task_row_no;

  let updated_alice3_row =
    List.hd (List.filter (fun row -> List.hd row = "alice3") !accounts_csv)
  in
  assert_equal [ "alice3"; "password3"; "pharmacist"; "[2]" ] updated_alice3_row;

  let updated_alice2_row =
    List.hd (List.filter (fun row -> List.hd row = "alice2") !accounts_csv)
  in
  assert_equal
    [ "alice2"; "password2"; "pharmacist"; "[2,1]" ]
    updated_alice2_row;

  Sys.remove temp_file_tasks;
  Sys.remove temp_file_accounts

(* ======================= TEST BLOCK ======================= *)

(** [test_hash] checks if a string is properly hashed to a correct value as
    expected *)
let test_hash _ =
  let data = "hello world" in
  let hashed = hash data in
  assert_bool "Hash should not be empty" (String.length hashed > 0);
  assert_bool "Hash should match expected length" (String.length hashed = 32)

(** [test_csv_to_string] checks if a hash is valid based on difficulty *)
let test_csv_to_string _ =
  let csv = [ [ "1"; "Task A" ]; [ "2"; "Task B" ] ] in
  let result = csv_to_string csv in
  let expected = "1,Task A\n2,Task B" in
  assert_equal expected result ~msg:"CSV string representation mismatch"

(** [test_mine_block] creates a block with specific parameters and verifies that
    the block's index, previous hash, hash validity, and nonce are correct. *)
let test_mine_block _ =
  let index = 1 in
  let timestamp = string_of_float (Unix.gettimeofday ()) in
  let tasks_csv = [ [ "1"; "Task A" ]; [ "2"; "Task B" ] ] in
  let previous_hash = "0" in
  let difficulty = 5 in
  let block = mine_block index timestamp tasks_csv previous_hash difficulty in
  assert_equal block.index index ~msg:"Block index mismatch";
  assert_equal block.previous_hash previous_hash ~msg:"Previous hash mismatch";
  assert_bool "Hash should be valid based on difficulty"
    (is_valid_hash block.hash difficulty);
  assert_bool "Nonce should be >= 0" (block.nonce >= 0)

(** [test_block_to_string _] creates a block with specific parameters, and
    verifying that the string representation of the block matches the expected
    string. *)
let test_block_to_string _ =
  let index = 1 in
  let timestamp = "1234567890.123" in
  let tasks_csv = [ [ "1"; "Task A" ]; [ "2"; "Task B" ] ] in
  let previous_hash = "abc123" in
  let nonce = 42 in
  let block =
    { index; timestamp; tasks_csv; previous_hash; nonce; hash = "dummy_hash" }
  in
  let result = block_to_string block in
  let expected = "1|1234567890.123|1,Task A\n2,Task B|abc123|42" in
  assert_equal expected result ~msg:"Block string representation mismatch"

(* ======================= TEST BLOCKCHAIN ======================= *)

(** [test_create_genesis_block _] creates a genesis block that is the first
    block in the blockchain with a specific difficulty, and verifying that the
    block's index, previous hash, hash validity, and tasks are correct. *)
let test_create_genesis_block _ =
  let difficulty = 2 in
  let genesis_block = create_genesis_block difficulty in
  assert_equal genesis_block.index 0 ~msg:"Genesis block index mismatch";
  assert_equal genesis_block.previous_hash "0"
    ~msg:"Genesis block previous hash mismatch";
  assert_bool "Genesis block hash should satisfy difficulty"
    (is_valid_hash genesis_block.hash difficulty);
  assert_equal genesis_block.tasks_csv [ [ "Genesis Block" ] ]
    ~msg:"Genesis block tasks mismatch"

(** [test_create_block] checks if the blocks are created properly and the verify
    that the block's index, previuos hash, has validity, and tasks are correct.
*)
let test_create_block _ =
  let difficulty = 2 in
  let blockchain = [ create_genesis_block difficulty ] in
  let tasks_csv = [ [ "1"; "Task A" ]; [ "2"; "Task B" ] ] in
  let new_block = create_block blockchain tasks_csv difficulty in
  assert_equal new_block.index 1 ~msg:"New block index mismatch";
  assert_equal new_block.previous_hash (List.hd blockchain).hash
    ~msg:"New block previous hash mismatch";
  assert_bool "New block hash should satisfy difficulty"
    (is_valid_hash new_block.hash difficulty);
  assert_equal new_block.tasks_csv tasks_csv ~msg:"New block tasks mismatch"

(** [test_validate_blockchain] creates a blockchain with multiple blocks and
    verifies that the blockchain is valid, then tampers with a block and
    verifies that the blockchain is invalid *)
let test_validate_blockchain _ =
  let difficulty = 2 in
  let genesis_block = create_genesis_block difficulty in
  let blockchain = ref [ genesis_block ] in
  let tasks_csv_1 = [ [ "1"; "Task A" ] ] in
  let tasks_csv_2 = [ [ "2"; "Task B" ] ] in
  let block1 = create_block !blockchain tasks_csv_1 difficulty in
  blockchain := block1 :: !blockchain;
  let block2 = create_block !blockchain tasks_csv_2 difficulty in
  blockchain := block2 :: !blockchain;
  (* Tamper with a block *)
  let tampered_block = { block1 with hash = "tampered_hash" } in
  let tampered_blockchain = tampered_block :: List.tl !blockchain in
  assert_bool "Invalid blockchain after tampering"
    (not (validate_blockchain tampered_blockchain))

(** [test_blockchain_to_json] creates a blockchain with multiple blocks and
    verifies that the JSON representation of the blockchain matches the expected
    JSON *)
let test_save_blockchain_to_file _ =
  let difficulty = 2 in
  let genesis_block = create_genesis_block difficulty in
  let blockchain = ref [ genesis_block ] in
  let tasks_csv_1 = [ [ "1"; "Task A" ] ] in
  let tasks_csv_2 = [ [ "2"; "Task B" ] ] in
  let block1 = create_block !blockchain tasks_csv_1 difficulty in
  blockchain := block1 :: !blockchain;
  let block2 = create_block !blockchain tasks_csv_2 difficulty in
  blockchain := block2 :: !blockchain;

  let temp_filename = "test_blockchain.csv" in
  save_blockchain_to_file !blockchain temp_filename;

  let read_json = Yojson.Basic.from_file temp_filename in
  let read_blocks = read_json |> to_list in

  (* Expected JSON to match with the actual json *)
  let expected_json = blockchain_to_json !blockchain in
  let expected_blocks = expected_json |> to_list in

  assert_equal ~cmp:( = ) ~printer:Yojson.Basic.pretty_to_string
    (`List expected_blocks) (`List read_blocks);

  Sys.remove temp_filename

(** [test_load_blockchain_from_file] creates a blockchain with multiple blocks,
    saves the blockchain to a file, loads the blockchain from the file, and
    verifies that the loaded blockchain matches the original blockchain *)
let test_load_blockchain_from_file _ =
  let difficulty = 2 in
  let genesis_block = create_genesis_block difficulty in
  let blockchain = ref [ genesis_block ] in
  let tasks_csv_1 = [ [ "1"; "Task A" ] ] in
  let tasks_csv_2 = [ [ "2"; "Task B" ] ] in

  let block1 = create_block !blockchain tasks_csv_1 difficulty in
  blockchain := block1 :: !blockchain;

  let block2 = create_block !blockchain tasks_csv_2 difficulty in
  blockchain := block2 :: !blockchain;

  let temp_filename = "test_blockchain.json" in
  save_blockchain_to_file !blockchain temp_filename;

  let loaded_blockchain = load_blockchain_from_file temp_filename in

  (* Expected JSON to match with the actual json *)
  let expected_json = blockchain_to_json !blockchain in
  let expected_blocks = expected_json |> to_list in

  let loaded_json = blockchain_to_json loaded_blockchain in
  let loaded_blocks = loaded_json |> to_list in

  assert_equal ~cmp:( = ) ~printer:Yojson.Basic.pretty_to_string
    (`List expected_blocks) (`List loaded_blocks);

  Sys.remove temp_filename

(* ======================= TEST TASK ======================= *)

(** [test_display_tasks_from_ids] checks if the tasks are correctly displayed
    with their votes, whether yes or no, for the users with different ids. *)
let test_display_tasks_from_ids _ =
  (* Mock CSV data for testing *)
  let tasks_csv =
    [
      [
        "1";
        "Diagnosis1";
        "Prescription1";
        "3";
        "Voter1,Voter2,Voter3";
        "2";
        "Voter4,Voter5";
      ];
      [
        "2";
        "Diagnosis2";
        "Prescription2";
        "5";
        "Voter6,Voter7,Voter8,Voter9,Voter10";
        "1";
        "Voter11";
      ];
      [ "3"; "Diagnosis3"; "Prescription3"; "0"; ""; "0"; "" ];
    ]
  in
  (* List of task IDs to display *)
  let task_ids = [ 1; 2 ] in

  (* Expected output *)
  let expected_output =
    "Task ID  | Diagnosis       | Prescription    | Yes Vote | No Vote\n\
     --------------------------------------------------------------\n\
     1        | Diagnosis1      | Prescription1   | 3        | 2       \n\
     2        | Diagnosis2      | Prescription2   | 5        | 1       \n"
  in

  (* Actual output from the function *)
  let actual_output = display_tasks_from_ids tasks_csv task_ids in

  (* Normalize whitespace for comparison *)
  let normalize str =
    String.trim str |> String.split_on_char '\n' |> List.map String.trim
    |> String.concat "\n"
  in

  assert_equal
    ~printer:(fun x -> x)
    (normalize expected_output)
    (normalize actual_output)

(** [test_display_tasks_without_votes] checks if the tasks are correctly
    displayed without the votes for different users with respective ids. *)
let test_display_tasks_without_votes _ =
  let tasks_csv =
    [
      [
        "1";
        "Diagnosis1";
        "Prescription1";
        "3";
        "Voter1,Voter2,Voter3";
        "2";
        "Voter4,Voter5";
      ];
      [
        "2";
        "Diagnosis2";
        "Prescription2";
        "5";
        "Voter6,Voter7,Voter8,Voter9,Voter10";
        "1";
        "Voter11";
      ];
      [ "3"; "Diagnosis3"; "Prescription3"; "0"; ""; "0"; "" ];
    ]
  in
  (* List of task IDs to display *)
  let task_ids = [ 1; 2 ] in
  let expected_output =
    "Task ID  | Diagnosis       | Prescription   \n\
     -----------------------------------------------------\n\
     1        | Diagnosis1      | Prescription1  \n\
     2        | Diagnosis2      | Prescription2  \n"
  in

  (* Actual output from the function *)
  let actual_output = display_tasks_without_votes tasks_csv task_ids in

  (* Normalize whitespace for comparison *)
  let normalize str =
    String.trim str |> String.split_on_char '\n' |> List.map String.trim
    |> String.concat "\n"
  in

  assert_equal
    ~printer:(fun x -> x)
    (normalize expected_output)
    (normalize actual_output)

(** [test_string_to_task_ids] checks if the string is properly converted to a
    list of task IDs *)
let test_string_to_task_ids _ =
  assert_equal [ 1; 2; 3 ] (string_to_task_ids "[1,2,3]");
  assert_equal [ 10; 20; 30 ] (string_to_task_ids "[10,20,30]");
  assert_equal [] (string_to_task_ids "[]")

(* ======================= TEST PATIENT ======================= *)

(** [test_display_prescription_statuses] checks if the prescription statuses are
    correctly displayed for the user with different tasks. *)
let test_display_prescription_statuses _ =
  (* Mock CSV reference *)
  let mock_csv = ref [] in

  (* User with no tasks *)
  let user_without_tasks = Patient.create_user "Alice" "heek" "pharmacist" [] in
  let expected_no_tasks =
    "Hello Alice. There are no current prescriptions waiting for approval \
     relevant to you.\n"
  in
  assert_equal
    ~printer:(fun s -> Printf.sprintf "Actual output: %s" s)
    ~msg:"User with no tasks should receive a no-tasks message"
    expected_no_tasks
    (display_prescription_statuses mock_csv user_without_tasks);

  (* User with multiple tasks *)
  let mock_csv_1 =
    ref
      [
        [ "1"; "Diagnosis1"; "Prescription1"; "10"; "0"; "0"; "0" ];
        [ "2"; "Diagnosis2"; "Prescription2"; "5"; "1"; "0"; "0" ];
        [ "3"; "Diagnosis3"; "Prescription3"; "0"; "0"; "0"; "0" ];
      ]
  in
  let user_with_tasks =
    Patient.create_user "Alice" "heek" "pharmacist" [ 1; 2; 3 ]
  in

  let expected_with_tasks =
    "Welcome Alice! Here is the status of your current prescriptions.\n"
    ^ "Task ID  | Diagnosis       | Prescription    | Yes Vote | No Vote \n\
       --------------------------------------------------------------\n\
       1        | Diagnosis1      | Prescription1   | 10       | 0       \n\
       2        | Diagnosis2      | Prescription2   | 5        | 0       \n\
       3        | Diagnosis3      | Prescription3   | 0        | 0       \n"
  in

  assert_equal
    ~printer:(fun s -> Printf.sprintf "Actual output: %s" s)
    ~msg:"User with multiple tasks should receive a multi-task status message"
    expected_with_tasks
    (display_prescription_statuses mock_csv_1 user_with_tasks);

  (* User with a single task *)
  let mock_csv_2 =
    ref [ [ "42"; "Diagnosis42"; "Prescription42"; "3"; "0"; "0"; "0" ] ]
  in
  let user_single_task =
    Patient.create_user "Alice" "Alice" "pharmacist" [ 42 ]
  in
  let expected_single_task =
    "Welcome Alice! Here is the status of your current prescriptions.\n"
    ^ "Task ID  | Diagnosis       | Prescription    | Yes Vote | No Vote \n\
       --------------------------------------------------------------\n\
       42       | Diagnosis42     | Prescription42  | 3        | 0       \n"
  in

  assert_equal
    ~printer:(fun s -> Printf.sprintf "Actual output: %s" s)
    ~msg:"User with a single task should receive a single-task status message"
    expected_single_task
    (display_prescription_statuses mock_csv_2 user_single_task);

  (* Edge case: User with tasks but empty CSV *)
  let empty_csv = ref [] in
  let user_empty_csv =
    Patient.create_user "Dana" "Dana" "pharmacist" [ 7; 8 ]
  in
  let expected_empty_csv =
    "Welcome Dana! Here is the status of your current prescriptions.\n"
    ^ "Task ID  | Diagnosis       | Prescription    | Yes Vote | No Vote \n\
       --------------------------------------------------------------\n\
       Task with ID 7 not found in tasks.\n\
       Task with ID 8 not found in tasks.\n"
  in
  assert_equal
    ~printer:(fun s -> Printf.sprintf "Actual output: %s" s)
    ~msg:"User with tasks and empty CSV should still generate task statuses"
    expected_empty_csv
    (display_prescription_statuses empty_csv user_empty_csv);
  (* Mock CSV with some tasks *)
  let mock_csv_3 =
    ref
      [
        [ "1"; "Diagnosis1"; "Prescription1"; "10"; "0"; "0" ];
        [ "2"; "Diagnosis2"; "Prescription2"; "5"; "0"; "0" ];
        [ "3"; "Diagnosis3"; "Prescription3"; "0"; "0"; "0" ];
      ]
  in

  (* User with a mix of valid and invalid task IDs *)
  let user_mixed_tasks =
    Patient.create_user "Charlie" "charlie123" "pharmacist" [ 1; 4; 2; 5 ]
  in

  (* Expected output *)
  let expected_mixed_tasks =
    "Welcome Charlie! Here is the status of your current prescriptions.\n"
    ^ "Task ID  | Diagnosis       | Prescription    | Yes Vote | No Vote \n\
       --------------------------------------------------------------\n\
       1        | Diagnosis1      | Prescription1   | 10       | 0       \n\
       Task with ID 4 not found in tasks.\n\
       2        | Diagnosis2      | Prescription2   | 5        | 0       \n\
       Task with ID 5 not found in tasks.\n"
  in

  (* Assert that the function produces the expected output *)
  assert_equal
    ~printer:(fun s -> Printf.sprintf "Actual output: %s" s)
    ~msg:
      "User with a mix of valid and invalid task IDs should receive \
       appropriate task statuses"
    expected_mixed_tasks
    (display_prescription_statuses mock_csv_3 user_mixed_tasks)

let suite =
  "test suite"
  >::: [
         "create_doctor" >:: test_create_doctor;
         "create_patient" >:: test_create_patient;
         "create_pharmacist" >:: test_create_pharmacist;
         "create_invalid" >:: test_create_invalid;
         "test_find_user" >:: test_find_user;
         "test_get_user_task_list" >:: test_get_user_task_list;
         "test_add_diagnosis_prescription" >:: test_add_diagnosis_prescription;
         "test_get_all_task_ids" >:: test_get_all_task_ids;
         "test_find_task_row" >:: test_find_task_row;
         "test_vote_on_task_core" >:: test_vote_on_task_core;
         "test_hash" >:: test_hash;
         "test_csv_to_string" >:: test_csv_to_string;
         "test_mine_block" >:: test_mine_block;
         "test_block_to_string" >:: test_block_to_string;
         "test_create_genesis_block" >:: test_create_genesis_block;
         "test_create_block" >:: test_create_block;
         "test_validate_blockchain" >:: test_validate_blockchain;
         "test_save_blockchain_to_file" >:: test_save_blockchain_to_file;
         "test_load_blockchain_from_file" >:: test_load_blockchain_from_file;
         "test_display_tasks_from_ids" >:: test_display_tasks_from_ids;
         "test_display_tasks_without_votes" >:: test_display_tasks_without_votes;
         "test_string_to_task_ids" >:: test_string_to_task_ids;
         "test_patient_modules" >:: test_display_prescription_statuses;
       ]

let () = run_test_tt_main suite
