(* type block = { index : int; timestamp : int; csv_data : Csv.t; tasks_csv :
   Csv.t; previous_hash : string; hash : string; }

   (*Storing the Blockchain*) val blockchain : block list ref

   (* A Hash Function that gives a hash-id for a block*) val sha256 : string ->
   string

   (*Creates and returns a new block that gets appended to the blockchain*) val
   create_block : int -> Csv.t -> Csv.t -> string -> block

   (*Validate Blockchain*) val validate_blockchain : block list -> bool *)
