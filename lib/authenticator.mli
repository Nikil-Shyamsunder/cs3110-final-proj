val load_users : string -> (string * string * string) list

val authenticate :
  string ->
  string ->
  (string * string * string) list ->
  (string * string * string) option
