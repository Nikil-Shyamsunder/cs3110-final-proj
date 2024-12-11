type block = {
  index : int;
  timestamp : string;
  accounts_csv : Csv.t;
  tasks_csv : Csv.t;
  previous_hash : string;
  hash : string;
}

module TaskCounter : sig
  val next : unit -> int
end

(* A Hash Function that gives a hash-id for a block*)
val hash : string -> string

(*Creates and returns a new block that gets appended to the blockchain*)
val create_block : block list -> Csv.t -> Csv.t -> block

(*Validate Blockchain*)
val validate_blockchain : block list -> bool
