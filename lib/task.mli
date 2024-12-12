type t
type voteTally

val load_tasks_from_csv : string -> t list
val display_tasks_from_ids : Csv.t -> int list -> string
val display_tasks_without_votes : Csv.t -> int list -> string
val string_to_task_ids : string -> int list
val task_id : t -> int
val diagnosis : t -> string
val prescription : t -> string
val vote_count : t -> voteTally
val yes_votes : voteTally -> int
val yes_voters : voteTally -> string list
val no_votes : voteTally -> int
val no_voters : voteTally -> string list
