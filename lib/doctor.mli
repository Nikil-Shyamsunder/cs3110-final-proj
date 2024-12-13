type t
(** AF:
    - A value of type [t] represents a user in the system. In this case, t
      represents a user-typed doctor, which inherits from the user file
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

val add_diagnosis_prescription :
  Csv.t ref -> Csv.t ref -> string -> string -> string -> string -> unit
(** [add_diagnosis_prescription accounts_csv prescriptions_csv doctor patient
     diagnosis]
    adds a diagnosis and prescription to the prescriptions CSV. *)
