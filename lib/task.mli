type t = Csv.t

val display_tasks_from_ids : t -> int list -> string
(** [display_tasks_from_ids csv task_ids] returns a string representation of the
    tasks with the given [task_ids]. *)

val display_tasks_without_votes : t -> int list -> string
(** [display_tasks_without_votes csv task_ids] returns a string representation
    of the tasks with the given [task_ids] that have not been voted on. *)

val string_to_task_ids : string -> int list
(** [string_to_task_ids str] returns a list of task ids from the string [str]. *)
