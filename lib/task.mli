
type t = Csv.t

val display_tasks_from_ids : t -> int list -> string
val display_tasks_without_votes : t -> int list -> string
val string_to_task_ids : string -> int list
val task_id : t -> int
val diagnosis : t -> string
val prescription : t -> string
val vote_count : t -> voteTally
val yes_votes : voteTally -> int
val yes_voters : voteTally -> string list
val no_votes : voteTally -> int
val no_voters : voteTally -> string list
