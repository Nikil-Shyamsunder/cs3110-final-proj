type t = {
  index : int;
  timestamp : string;
  tasks_csv : Csv.t;
  previous_hash : string;
  nonce : int;
  hash : string;
}

val hash : string -> string
(** Hashes a string using Digest.

    Parameters:
    - [input] : a string to be hashed.

    Preconditions:
    - The input string must not be empty.

    Postconditions:
    - Returns a hexadecimal representation of the hash.
    - The output is always 32 characters long, representing a 128-bit hash. *)

val block_to_string : t -> string
(** Converts a block to a string for hashing.

    Parameters:
    - [block] : a block of type [t].

    Preconditions:
    - The block fields must be well-formed (e.g., valid timestamp, non-empty
      hash).

    Postconditions:
    - Returns a string representation of the block that combines all its fields
      in a deterministic order.
    - The output is suitable for hashing to generate the block's hash. *)

val is_valid_hash : string -> int -> bool
(** Checks if a hash is valid for a given difficulty.

    Parameters:
    - [hash] : the hash string to validate.
    - [difficulty] : the number of leading zeroes required for the hash to be
      valid.

    Preconditions:
    - [hash] must be a valid hexadecimal string.
    - [difficulty] must be a non-negative integer.

    Postconditions:
    - Returns [true] if the hash has at least [difficulty] leading zeroes;
      otherwise, returns [false]. *)

val mine_block : int -> string -> Csv.t -> string -> int -> t
(** Mines a new block with the given parameters and difficulty. *)
