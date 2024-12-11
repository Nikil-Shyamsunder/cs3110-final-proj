type t
(** Type representing a user *)

val create_user : string -> string -> string -> int list -> t
(** [create_user u p role_op] creates a new user with username [u], password
    [p], and a role specified by the string [role_op]. The [task_list] is
    initialized as an empty list. Raises: [Failure "input invalid"] if [role_op]
    is not a valid role string. *)

val role : t -> string
(** [role acc] returns the role of the user [acc]. *)

val username : t -> string
val tasks : t -> int list
val vote_on_task_core : Csv.t ref -> Csv.t ref -> t -> int -> string -> unit
val get_all_task_ids : Csv.t ref -> int list
val find_task_row : Csv.t ref -> int -> string list option
