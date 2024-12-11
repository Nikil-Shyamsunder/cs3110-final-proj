open OUnit2
open Csv
open Digest
open Sys
open Prescription_validator.Block

(* Test case for hashing a string *)
let test_hash _ =
  let data = "hello world" in
  let hashed = hash data in
  assert_bool "Hash should not be empty" (String.length hashed > 0);
  assert_bool "Hash should match expected length" (String.length hashed = 32)

(* Test case for converting CSV to string *)
let test_csv_to_string _ =
  let csv = [ [ "1"; "Task A" ]; [ "2"; "Task B" ] ] in
  let result = csv_to_string csv in
  let expected = "1,Task A\n2,Task B" in
  assert_equal expected result ~msg:"CSV string representation mismatch"

(* Test case for mining a block *)
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

(* Test case for block string conversion *)
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

(* Test suite *)
let suite =
  "Block Tests"
  >::: [
         "test_hash" >:: test_hash;
         "test_csv_to_string" >:: test_csv_to_string;
         "test_mine_block" >:: test_mine_block;
         "test_block_to_string" >:: test_block_to_string;
       ]

let () = run_test_tt_main suite
