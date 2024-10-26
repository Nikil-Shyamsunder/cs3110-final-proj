module Account = struct end

module Pharmacist = struct
  include Patient
end

module Patient = struct
  include Patient
end

module Doctor = struct
  include Patient
end
