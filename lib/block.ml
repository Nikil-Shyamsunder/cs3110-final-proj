(* open Digest open Sys open Csv

   type block = { index : int; timestamp : string; accounts_csv : Csv.t;
   tasks_csv : Csv.t; previous_hash : string; hash : string; }

   module TaskCounter = struct let counter = ref 0

   let next () = incr counter; !counter end

   let hash data = Digest.(to_hex (string data))

   let csv_to_string csv = csv |> List.map (fun row -> String.concat "," row) |>
   String.concat "\n"

   let create_block index accounts_csv (tasks_csv : Csv.t) previous_hash = let
   timestamp = string_of_float (Sys.time ()) in

   let block_data = Printf.sprintf "%d|%s|%s|%s|%s" index timestamp
   (csv_to_string accounts_csv) (csv_to_string tasks_csv) previous_hash in let
   hash_code = hash block_data in (* let new_id = get_last_task_id tasks_csv in
   *) { index = new_id; timestamp; accounts_csv; tasks_csv; previous_hash; hash
   = hash_code; } *)
