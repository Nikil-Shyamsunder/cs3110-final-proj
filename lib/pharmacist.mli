type t
(** AF:
    - A value of type [t] represents a user in the system. In this case, the
      type t from account is inherited because this is a user
    - The user has a unique [username] and [password] for authentication.
    - Each user is assigned a [role], which defines their permissions or
      function within the system (e.g., "doctor", "patient", "admin").
    - The user maintains a [task_list], which contains a list of task
      identifiers associated with their activities in the system.

    RI:
    - The [username] must be a non-empty string and unique across all users in
      the system.
    - The [password] must be a non-empty string.
    - The [role] must be one of a predefined set of valid role strings (e.g.,
      "doctor", "patient", "admin").
    - The [task_list] must only contain unique non-negative integers.
    - Users must have consistent data when accessed or modified through module
      functions. *)

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
