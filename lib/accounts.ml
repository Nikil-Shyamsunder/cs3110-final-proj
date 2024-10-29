module Account = struct
  type role =
    | Doctor
    | Pharmacist
    | Patient

  type t = {
    role : role;
    username : string;
    password : string;
    task_list : int list;
  }
  
  let role_display = function
    | Doctor -> "Doctor"
    | Pharmacist -> "Pharmacist"
    | Patient -> "Patient"
  let role_to_string = function
    | Patient -> "patient"
    | Doctor -> "doctor"
    | Pharmacist -> "pharmacist"

  let string_to_role = function
    | "patient" -> Patient
    | "doctor" -> Doctor
    | "pharmacist" -> Pharmacist
    | _ -> failwith "input invalid"

  let create_user (u : string) (p : string) (role_op : string) =
    {
      username = u;
      password = p;
      role = string_to_role role_op;
      task_list = [];
    }
end

module Pharmacist = struct
  include Account
end

module Patient = struct
  include Account
end

module Doctor = struct
  include Account
end
