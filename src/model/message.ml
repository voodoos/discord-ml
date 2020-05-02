(* TODO incomplete *)
type t = {
  id : Snowflake.t;
  channel_id : Snowflake.t;
  author : User.t;
  content : string;
}
[@@deriving yojson { strict = false; exn = true }]
