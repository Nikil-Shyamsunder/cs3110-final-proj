open Block

type t
(** Type for a blockchain *)

val create_genesis_block : int -> Block.t
(** [create_genesis_block difficulty] creates the genesis block with the given
    difficulty *)

val create_block : t -> Csv.t -> int -> Block.t
(** [create_block blockchain transactions difficulty] creates a new block with
    the given transactions and difficulty *)

val validate_blockchain : t -> bool
(** [validate_blockchain blockchain] validates the blockchain *)

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
(** [blockchain_to_json blockchain] converts a blockchain to a json object *)

val blockchain_of_json : Yojson.Basic.t -> Block.t list
(** [blockchain_of_json blockchain] converts a json object to a blockchain *)

val save_blockchain_to_file : t -> string -> unit
(** [save_blockchain_to_file blockchain filename] saves the blockchain to a file
    that is filename *)

val load_blockchain_from_file : string -> t
(** [load_blockchain_from_file filename] loads the blockchain from a file called
    filename here *)

val latest_tasks : t -> Csv.t
(** [latest_tasks blockchain] are the latest tasks on the blockchain *)

val append_block : t -> Block.t -> t
(** [append_block blockchain block] appends [block] to the front of this
    [blockchain].*)

val empty : t
(** [empty] is the empty blockchain. *)

val list_repr : t -> Block.t list
(** [list_repr blockchain] is a list of Blocks representing this [blockchain]*)

val chain_of_list : Block.t list -> t
(** [chain_of_list lst blockchain] is the Blockchain.t representation of the
    list *)
