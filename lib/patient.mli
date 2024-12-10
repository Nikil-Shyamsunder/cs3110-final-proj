type role
(** Type representing user roles *)

type t
(** Type representing a user *)

val create_user : string -> string -> string -> int list -> t
(** [create_user u p role_op] creates a new user with username [u], password
    [p], and a role specified by the string [role_op]. The [task_list] is
    initialized as an empty list. Raises: [Failure "input invalid"] if [role_op]
    is not a valid role string. *)

val role : t -> role
(** [role acc] returns the role of the user [acc]. *)

val display_prescription_statuses : Csv.t ref -> t -> unit
