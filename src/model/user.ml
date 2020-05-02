(* TODO incomplete *)
type t = {
  id : Snowflake.t;
  username : string;
  discriminator : string;
  avatar : string option;
}
[@@deriving yojson { strict = false; exn = true }]
