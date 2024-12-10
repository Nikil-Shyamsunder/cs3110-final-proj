(** Type representing user roles *)
type role =
  | Doctor
  | Pharmacist
  | Patient

type t = {
  role : role;
  username : string;
  password : string;
  task_list : int list;
}
(** Type representing a user *)

val role_display : role -> string
(** [role_display role] returns a user-friendly string representation of the
    given [role]. *)

val role_to_string : role -> string
(** [role_to_string role] converts a [role] to its lowercase string
    representation. *)

val string_to_role : string -> role
(** [string_to_role s] converts a lowercase string [s] into a [role]. Raises:
    [Failure "input invalid"] if [s] is not a valid role string. *)

val create_user : string -> string -> string -> t
(** [create_user u p role_op] creates a new user with username [u], password
    [p], and a role specified by the string [role_op]. The [task_list] is
    initialized as an empty list. Raises: [Failure "input invalid"] if [role_op]
    is not a valid role string. *)

val role : t -> role
(** [role acc] returns the role of the user [acc]. *)
