open Digest
open Sys
open Csv

type block = {
  index : int;
  timestamp : string;
  tasks_csv : Csv.t;
  previous_hash : string;
  hash : string;
}

module TaskCounter = struct
  let counter = ref 0

  let next () =
    incr counter;
    !counter
end

let hash data = Digest.(to_hex (string data))

let csv_to_string csv =
  csv |> List.map (fun row -> String.concat "," row) |> String.concat "\n"

let create_block (blockchain : 'a ref) (tasks_csv : Csv.t) =
  let new_index = TaskCounter.next () in
  let timestamp = string_of_float (Sys.time ()) in
  let previous_hash =
    match !blockchain with
    | [] -> string_of_int 0
    | last_block :: _ -> last_block.hash
  in
  let block_data =
    Printf.sprintf "%d|%s|%s|%s" new_index timestamp (csv_to_string tasks_csv)
      previous_hash
  in
  let hash_code = hash block_data in
  { index = new_index; timestamp; tasks_csv; previous_hash; hash = hash_code }

let validate_blockchain blockchain =
  let rec validate_chain = function
    | [] | [ _ ] -> true (* A single block or empty chain is valid *)
    | b1 :: (b2 :: _ as rest) ->
        b2.previous_hash = b1.hash && validate_chain rest
  in
  validate_chain blockchain
