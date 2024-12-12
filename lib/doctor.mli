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

val add_diagnosis_prescription :
  Csv.t ref -> Csv.t ref -> string -> string -> string -> string -> unit
(** [add_diagnosis_prescription accounts_csv prescriptions_csv doctor patient
     diagnosis] adds a diagnosis and prescription to the prescriptions CSV. *)
