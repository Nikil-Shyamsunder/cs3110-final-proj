open Digest
open Sys
open Csv

type t = {
  index : int;
  timestamp : string;
  tasks_csv : Csv.t;
  previous_hash : string;
  nonce : int;
  hash : string;
}
(** Abstract type for a block *)

let create_block index timestamp tasks_csv previous_hash nonce hash =
  { index; timestamp; tasks_csv; previous_hash; nonce; hash }

(** Getter functions for each field of the block *)
let index block = block.index

let timestamp block = block.timestamp
let tasks_csv block = block.tasks_csv
let previous_hash block = block.previous_hash
let nonce block = block.nonce
let get_hash block = block.hash

(* Hashing utilities *)
let hash data = Digest.to_hex (Digest.string data)

(* Converts CSV data into a string for hashing purposes *)
let csv_to_string csv =
  csv |> List.map (fun row -> String.concat "," row) |> String.concat "\n"

(* Converts a block into a string for hashing *)
let block_to_string block =
  Printf.sprintf "%d|%s|%s|%s|%d" block.index block.timestamp
    (csv_to_string block.tasks_csv)
    block.previous_hash block.nonce

(* Validates a hash based on the given difficulty (number of leading zeros) *)
let is_valid_hash hash difficulty =
  String.sub hash 0 difficulty = String.make difficulty '0'

(* Mines a block by finding a valid nonce *)
let mine_block index timestamp tasks_csv previous_hash difficulty =
  let rec find_nonce nonce =
    let candidate =
      Printf.sprintf "%d|%s|%s|%s|%d" index timestamp (csv_to_string tasks_csv)
        previous_hash nonce
    in
    (* print_endline candidate; *)
    let hash_code = hash candidate in
    if is_valid_hash hash_code difficulty then
      (* let () = print_endline hash_code in *)
      (nonce, hash_code)
    else find_nonce (nonce + 1)
  in
  let nonce, final_hash = find_nonce 0 in
  { index; timestamp; tasks_csv; previous_hash; nonce; hash = final_hash }
