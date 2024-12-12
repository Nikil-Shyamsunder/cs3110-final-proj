val load_users : string -> (string * string * string * string) list
(** [load_users filename] loads the users from the file with the given
    [filename]. *)

val authenticate :
  string ->
  string ->
  (string * string * string * string) list ->
  (string * string * string * string) option
(** [authenticate username password users] returns the user with the given
    [username] and [password] if it exists in the list of [users]. *)
