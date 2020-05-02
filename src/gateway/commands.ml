(*
https://discordapp.com/developers/docs/topics/gateway#commands-and-events-gateway-commands
*)

type heartbeat = {
  op : int;
  (* 1 *)
  d : int option;
      (*  The inner [d] key is  the last sequence number—[s]—received by the client. If you have not yet received one, send null. *)
}
[@@deriving yojson { strict = true }]

let beat seq = heartbeat_to_yojson { op = 1; d = seq }

type identify_connection_properties = {
  os : string;
  (* your operating system *)
  browser : string;
  (* your library name *)
  device : string; (* your library name *)
}
[@@deriving yojson]

type identify_struct = {
  token : string;
  (* authentication token *)
  properties : identify_connection_properties;
  (* authentication token *)
  compress : bool; [@default false]
  (* whether this connection supports
     compression of packets *)
  large_threshold : int; [@default 50]
      (*  TODO shard presence guild_subscriptions intents *)
}
[@@deriving yojson { strict = false }]

type identify = { op : int; (* 2 *)
                            d : identify_struct }
[@@deriving yojson { strict = true }]

let identify token =
  identify_to_yojson
    {
      op = 2;
      d =
        {
          token;
          properties = { os = "linux"; browser = "disco"; device = "disco" };
          compress = false;
          large_threshold = 50;
        };
    }
