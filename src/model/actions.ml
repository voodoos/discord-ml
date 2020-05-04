module Create_message = struct
  type payload = {
    content : string;
        (* TODO https://discordapp.com/developers/docs/resources/channel#create-message *)
    tts : bool;
  }
  [@@deriving yojson { strict = false }]

  let run ~payload channel_id =
    let endp = Http.Endpoints.channel_messages (Snowflake.to_int channel_id) in
    Http.Client.request (Post (endp, payload_to_yojson payload))
end
