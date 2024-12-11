type t

val load_tasks_from_csv : string -> t list
val display_tasks_from_ids : Csv.t -> int list -> string
val display_tasks_without_votes : Csv.t -> int list -> string
val string_to_task_ids : string -> int list
