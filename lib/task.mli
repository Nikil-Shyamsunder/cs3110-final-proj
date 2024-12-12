type t = Csv.t

val display_tasks_from_ids : t -> int list -> string
val display_tasks_without_votes : t -> int list -> string
val string_to_task_ids : string -> int list
