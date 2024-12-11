open Block

type t = Block.t list
(** Type for a blockchain *)

val create_genesis_block : int -> Block.t
(** Creates the genesis block with the given difficulty *)

val create_block : t -> Csv.t -> int -> Block.t
(** Creates a new block with data, linked to the last block in the chain *)

val validate_blockchain : t -> bool
(** Validates the integrity of the blockchain *)

val blockchain_to_json :
  Block.t list ->
  [> `List of
     [> `Assoc of
        (string
        * [> `Int of int
          | `List of [> `List of [> `String of string ] list ] list
          | `String of string
          ])
        list
     ]
     list
  ]

val save_blockchain_to_file : t -> string -> unit
val load_blockchain_from_file : string -> t
