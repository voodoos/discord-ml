open Lwt.Infix

module Create_message = struct
  type payload = {
    content : string;
    (* TODO https://discordapp.com/developers/docs/resources/channel#create-message *)
    nonce : string option; [@default None]
    tts : bool;
  }
  [@@deriving yojson { strict = false }]

  let run ~payload channel_id =
    let endp = Http.Endpoints.channel_messages (Snowflake.to_int channel_id) in
    let payload = payload_to_yojson payload in
    Http.Client.request (Post (endp, payload)) >|= fun resp ->
    Yojson.Safe.from_string resp |> Message.of_yojson_exn
end

module Delete_message = struct
  let run channel_id message_id =
    let endp =
      Http.Endpoints.channel_message
        (Snowflake.to_int channel_id)
        (Snowflake.to_int message_id)
    in
    Http.Client.request (Delete endp)
end
