include Account

(* Function to add a new record *)
let add_diagnosis_prescription path diagnosis prescription =
  let csv = Csv.load path in
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
  Csv.save path updated_csv;
  Printf.printf "Record added: [%s]\n" (String.concat ", " new_record)
