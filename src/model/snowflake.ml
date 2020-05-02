(* from disml *)

type t = Int64.t

let of_yojson_exn d = Yojson.Safe.Util.to_string d |> Int64.of_string

let of_yojson d =
  try Ok (of_yojson_exn d)
  with Yojson.Safe.Util.Type_error (why, _) -> Error why

let to_yojson s : Yojson.Safe.t = `String (Int64.to_string s)

let to_string (t : t) = Int64.to_string t

let to_int (t : t) = Int64.to_int t

let timestamp snowflake = (snowflake lsr 22) + 1_420_070_400_000
