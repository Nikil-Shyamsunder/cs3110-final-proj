include Account

let add_diagnosis_prescription (tasks_csv_ref : Task.t ref)
    (accounts_csv_ref : Csv.t ref) (doctor : string) (patient : string)
    diagnosis prescription =
  let csv = Task.to_csv !tasks_csv_ref in
  let new_id =
    (* Compute the new ID by finding the maximum ID in the existing rows *)
    List.fold_left
      (fun acc row ->
        try max acc (int_of_string (List.hd row)) with Failure _ -> acc)
      0 csv
    + 1
  in
  (* Create a new record for the diagnosis and prescription *)
  let new_record =
    [ string_of_int new_id; diagnosis; prescription; "0"; "[]"; "0"; "[]" ]
  in
  (* Append the new record to the CSV data *)
  tasks_csv_ref := Task.of_csv (csv @ [ new_record ]);

  (* Update the tasks column for the patient and doctor *)
  update_user_tasks accounts_csv_ref patient new_id;
  update_user_tasks accounts_csv_ref doctor new_id
