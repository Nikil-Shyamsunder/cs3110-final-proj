open Digest

type block = {
  index : int;
  timestamp : string;
  accounts_csv : Csv.t;
  tasks_csv : Csv.t;
  previous_hash : string;
  hash : string;
}

let sha256 data = Digest.SHA256.to_hex (Digest.SHA256.digest_string data)
