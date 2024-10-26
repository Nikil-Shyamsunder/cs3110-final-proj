module Account = struct end

module Pharmacist = struct
  include Account
end

module Patient = struct
  include Account
end

module Doctor = struct
  include Account
end
