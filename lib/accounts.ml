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
