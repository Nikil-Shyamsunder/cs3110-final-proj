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
(** [username acc] returns the username of the user [acc]. *)

val tasks : t -> int list
(** [tasks acc] returns the task list of the user [acc]. *)

val find_user : string -> string -> string list option
(** [find_user accounts_path username] finds a user in the accounts CSV by
    username. *)

val get_user_task_list : string -> string -> int list option
(** [get_user_task_list accounts_path username] gets a user's task list from the
    accounts CSV. *)

val update_user_tasks : Csv.t ref -> string -> int -> unit
(** [update_user_tasks accounts_csv username task_id] updates the final column
    of a user's row in the accounts CSV. *)
