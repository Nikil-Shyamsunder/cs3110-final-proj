include Account

(* Helper function to update the final column of a user's row *)
let update_user_tasks (accounts_csv : Csv.t) username task_id =
  List.map
    (fun row ->
      match row with
      | [ u; pw; role; tasks ] when u = username ->
          let updated_tasks =
            (* Parse the existing list, add the task_id, and convert back to
               string *)
            let tasks_list =
              try
                String.sub (String.trim tasks) 1 (String.length tasks - 2)
                (* Remove the square brackets *)
                |> String.split_on_char ',' (* Split by comma *)
                |> List.map String.trim
                |> List.filter (fun x -> x <> "")
                (* Remove empty strings *)
              with _ -> []
            in
            let new_tasks = task_id :: tasks_list in
            Printf.sprintf "[%s]" (String.concat "," new_tasks)
          in
          [ u; pw; role; updated_tasks ]
      | _ -> row)
    accounts_csv

(* Main function to add a new record and update accounts *)
let add_diagnosis_prescription tasks_path accounts_path doctor patient diagnosis
    prescription =
  let csv = Csv.load tasks_path in
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
  let updated_csv = csv @ [ new_record ] in
  (* Write the updated CSV back to the file *)
  Csv.save tasks_path updated_csv;

  (* Load the accounts CSV *)
  let accounts_csv = Csv.load accounts_path in

  (* Update the tasks column for the patient and doctor *)
  let _updated_accounts_csv =
    update_user_tasks accounts_csv patient (string_of_int new_id)
  in
  let updated_accounts_csv =
    update_user_tasks _updated_accounts_csv doctor (string_of_int new_id)
  in

  (* Save the updated accounts CSV *)
  Csv.save accounts_path updated_accounts_csv;

  Printf.printf "Record added: [%s]\n" (String.concat ", " new_record)
