open OUnit2
open Prescription_validator.Blockchain
open Prescription_validator.Block

(* Test case for creating the genesis block *)
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

(* Test case for creating a new block *)
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

(* Test case for validating a blockchain *)
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
  assert_bool "Valid blockchain" (validate_blockchain !blockchain);

  (* Tamper with a block *)
  let tampered_block = { block1 with hash = "tampered_hash" } in
  let tampered_blockchain = tampered_block :: List.tl !blockchain in
  assert_bool "Invalid blockchain after tampering"
    (not (validate_blockchain tampered_blockchain))

let test_save_blockchain_to_file _ = ()

(* Test suite *)
let suite =
  "Blockchain Tests"
  >::: [
         "test_create_genesis_block" >:: test_create_genesis_block;
         "test_create_block" >:: test_create_block;
         "test_validate_blockchain" >:: test_validate_blockchain;
         "test_save_blockchain_to_file" >:: test_save_blockchain_to_file;
       ]

let () = run_test_tt_main suite
