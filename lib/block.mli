type t = {
  index : int;
  timestamp : string;
  tasks_csv : Csv.t;
  previous_hash : string;
  nonce : int;
  hash : string;
}

val hash : string -> string
(** Hashes a string using Digest *)

val block_to_string : t -> string
(** Converts a block to a string for hashing *)

val is_valid_hash : string -> int -> bool
(** Checks if a hash is valid for a given difficulty *)

val mine_block : int -> string -> Csv.t -> string -> int -> t
(** Mines a new block with the given parameters and difficulty *)

val csv_to_string : Csv.t -> string
