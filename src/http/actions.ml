open Lwt.Infix
open Model

module Get_channel_messages = struct
  type payload = {
    (* TODO https://discord.com/developers/docs/resources/channel#get-channel-messages *)
    limit : int option;
  }

  let to_query payload =
    List.concat
      [
        payload.limit
        |> Option.map (fun i -> ("limit", string_of_int i))
        |> Option.to_list;
      ]

  let run ~payload channel_id =
    let endp = Endpoints.channel_messages (Snowflake.to_int channel_id) in
    let uri = Endpoints.to_uri ~query:(to_query payload) endp in
    Logs.debug (fun m -> m "URI = %s" (Uri.to_string uri));
    Client.request (Get uri) >|= fun resp ->
    Yojson.Safe.from_string resp |> fun yjson ->
    let l = Yojson.Safe.Util.to_list yjson in
    List.map Message.of_yojson_exn l
end

module Create_message = struct
  type payload = {
    content : string;
    (* TODO https://discordapp.com/developers/docs/resources/channel#create-message *)
    nonce : string option; [@default None]
    tts : bool;
  }
  [@@deriving yojson { strict = false }]

  let run ~payload channel_id =
    let endp = Endpoints.channel_messages (Snowflake.to_int channel_id) in
    let payload = payload_to_yojson payload in
    Client.request (Post (endp, payload)) >|= fun resp ->
    Yojson.Safe.from_string resp |> Message.of_yojson_exn
end

module Edit_message = struct
  type payload = {
    content : string;
    (* TODO https://discordapp.com/developers/docs/resources/channel#create-message *)
    nonce : string option; [@default None]
    tts : bool;
  }
  [@@deriving yojson { strict = false }]

  let run ~payload channel_id message_id =
    let endp =
      Endpoints.channel_message
        (Snowflake.to_int channel_id)
        (Snowflake.to_int message_id)
    in
    let payload = payload_to_yojson payload in
    Client.request (Patch (endp, payload)) >|= fun resp ->
    Yojson.Safe.from_string resp |> Message.of_yojson_exn
end

module Delete_message = struct
  let run channel_id message_id =
    let endp =
      Endpoints.channel_message
        (Snowflake.to_int channel_id)
        (Snowflake.to_int message_id)
    in
    Client.request (Delete endp)
end
