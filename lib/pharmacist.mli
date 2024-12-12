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

val get_all_task_ids : Csv.t ref -> int list
(** [get_all_task_ids tasks_csv] gets the task IDs from tasks CSV *)

val find_task_row : Csv.t ref -> int -> string list option
(** [find_task_row tasks_csv] finds a task row in the csv file of the available
    tasks by task ID *)

val vote_on_task_core : Csv.t ref -> Csv.t ref -> t -> int -> string -> unit
(** [vote_on_task_core accounts_csv tasks_csv user task_id vote] records a vote
    on a task and updates the user's task list. *)
