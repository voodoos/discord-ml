(*
https://discordapp.com/developers/docs/topics/gateway#commands-and-events-gateway-events
*)

type hello = { heartbeat_interval : int }
[@@deriving yojson { strict = false; exn = true }]

module Ready = struct
  type t = {
    version : int; [@key "v"]
    user : Model.User.t;
    (* ; private_channels: Channel_id.t list *)
    (* ; guilds: Guild_t.unavailable list *)
    session_id : string;
  }
  [@@deriving yojson { strict = false; exn = true }]
end
