type t
(** AF: - A value of type [t] represents a task
    - The task has a csv format where each row corresponds to a task with
      specific attributes.
    - Each row contains: - Task ID (unique integer) - Diagnosis (string) -
      Prescription (string) - Yes Vote (integer) - No Vote (integer) - The rows
      collectively represent the tasks stored in the system, with specific
      operations for displaying tasks and processing task-related information.

    RI: - Each task has an id, its corresponding users, and its votes. - Task
    IDs in the table must be unique. - The functions should handle invalid task
    IDs gracefully, either by reporting an error or skipping the invalid row. -
    Strings representing numeric values (e.g., Task ID, Yes Vote, No Vote) must
    be convertible to integers without errors. - Task-related functions assume
    task IDs referenced as inputs exist in the table or provide appropriate
    error messages if not found. *)

val of_csv : Csv.t -> t
(** [of_csv csv] creates a task list from a CSV. *)

val to_csv : t -> Csv.t
(** [to_csv t] converts a task list to a CSV. *)

val display_tasks_from_ids : t -> int list -> string
(** [display_tasks_from_ids csv task_ids] returns a string representation of the
    tasks with the given [task_ids]. *)

val display_tasks_without_votes : t -> int list -> string
(** [display_tasks_without_votes csv task_ids] returns a string representation
    of the tasks with the given [task_ids] that have not been voted on. *)

val string_to_task_ids : string -> int list
(** [string_to_task_ids str] returns a list of task ids from the string [str]. *)
