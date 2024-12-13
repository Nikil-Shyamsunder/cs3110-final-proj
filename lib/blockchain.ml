open Block
open Yojson.Basic

type t = Block.t list
(** AF:
    - A value of type [t] represents a blockchain, which is a sequence of
      blocks.
    - Each block in the blockchain is linked to the previous block by the
      [previous_hash] field, forming a chain.
    - The first block in the chain is the genesis block, which serves as the
      foundation of the blockchain.

    RI:
    - The blockchain must have at least one block (the genesis block).
    - Each block's [previous_hash] must match the [hash] of the preceding block.
    - The hash of each block must correctly match the hash of its serialized
      data as computed by [block_to_string].
    - The genesis block's [previous_hash] must be "0".
    - The difficulty requirement (e.g., leading zeros in the hash) must be
      satisfied for every block.
    - The blocks must be ordered by their [index], starting from 0 and
      increasing sequentially. *)

type t = Block.t list

let create_genesis_block difficulty =
  let timestamp = string_of_float (Sys.time ()) in
  mine_block 0 timestamp [ [ "Genesis Block" ] ] "0" difficulty

let create_block (blockchain : t) tasks_csv difficulty =
  let index = List.length blockchain in
  let timestamp = Sys.time () in
  let previous_hash =
    match blockchain with
    | [] -> string_of_int 0
    | last_block :: _ -> last_block.hash
  in
  let timestamp = string_of_float timestamp in
  mine_block index timestamp tasks_csv previous_hash difficulty

let validate_blockchain (blockchain : t) : bool =
  let rec validate_chain = function
    | [] | [ _ ] ->
        true (* A blockchain with one or no blocks is always valid *)
    | current_block :: previous_block :: rest ->
        let computed_hash = hash (block_to_string current_block) in
        (* Check if the hash matches and if the previous hash links correctly *)
        if
          current_block.hash = computed_hash
          && current_block.previous_hash = previous_block.hash
          && is_valid_hash current_block.hash 2
        then validate_chain (previous_block :: rest)
        else false
  in
  validate_chain blockchain

(** [block_to_json block] is a helper function to convert the block data into a
    json with data like index, timestamp, tasks_csv, previous_hash, nonce, hash. *)
let block_to_json (block : Block.t) =
  `Assoc
    [
      ("index", `Int block.index);
      ("timestamp", `String block.timestamp);
      ( "tasks_csv",
        `List
          (List.map
             (fun row -> `List (List.map (fun col -> `String col) row))
             block.tasks_csv) );
      ("previous_hash", `String block.previous_hash);
      ("nonce", `Int block.nonce);
      ("hash", `String block.hash);
    ]

(** [block_of_json json] converts a json object into a block. *)
let block_of_json json =
  let open Yojson.Basic.Util in
  {
    index = json |> member "index" |> to_int;
    timestamp = json |> member "timestamp" |> to_string;
    tasks_csv =
      json |> member "tasks_csv" |> to_list
      |> List.map (fun row -> row |> to_list |> List.map to_string);
    previous_hash = json |> member "previous_hash" |> to_string;
    nonce = json |> member "nonce" |> to_int;
    hash = json |> member "hash" |> to_string;
  }

let blockchain_to_json blockchain = `List (List.map block_to_json blockchain)

let save_blockchain_to_file blockchain filename =
  let json = blockchain_to_json blockchain in
  let oc = open_out filename in
  Yojson.Basic.pretty_to_channel oc json;
  close_out oc

let blockchain_of_json json =
  json |> Yojson.Basic.Util.to_list |> List.map block_of_json

let load_blockchain_from_file filename =
  let ic = open_in filename in
  let json = Yojson.Basic.from_channel ic in
  close_in ic;
  blockchain_of_json json

let latest_tasks blockchain = (List.hd blockchain).tasks_csv
let append_block blockchain block = block :: blockchain
let empty = []
let list_repr blockchain = blockchain
let chain_of_list lst = lst
