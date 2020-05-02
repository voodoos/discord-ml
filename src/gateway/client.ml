open! Compat
open Lwt.Infix
open Websocket
module Mvar = Lwt_mvar
open State
module Cache = Model.Cache

module Make (Handlers : Handlers.S) = struct
  let dispatch ~payload state =
    let module Json = Yojson.Safe.Util in
    match Json.member "t" payload with
    | `String "READY" ->
        let ready = Json.member "d" payload |> Events.Ready.of_yojson_exn in
        Logs.debug (fun m ->
            m "Dispatch: READY (version %d, user %s)" ready.version
              ready.user.username);
        Lwt.return
          {
            state with
            session_id = Some ready.session_id;
            cache = Some Cache.{ user = ready.user };
          }
    | `String "MESSAGE_CREATE" ->
        let message = Json.member "d" payload |> Model.Message.of_yojson_exn in
        let cache = State.get_cache_exn state in
        Handlers.on_message ~cache message >|= fun cache ->
        { state with cache = Some cache }
    (*
  {"type":0,"tts":false,"timestamp":"2020-05-01T17:17:12.256000+00:00","pinned":false,"nonce":"705830195059228672","mentions":[],"mention_roles":[],"mention_everyone":false,"member":{"roles":[],"mute":false,"joined_at":"2020-04-13T10:42:56.850000+00:00","hoisted_role":null,"deaf":false},"id":"705830195323469844","flags":0,"embeds":[],"edited_timestamp":null,"content":"test","channel_id":"700051923390169149","author":{"username":"vds","public_flags":128,"id":"213231389275979776","discriminator":"3945","avatar":"b1a74accfac7022e91ff2da18f0b97da"},"attachments":[],"guild_id":"699207995749302353"}
  *)
    | _ ->
        Logs.debug (fun m -> m "Unhandled dispatch.");
        Lwt.return state

  let handle_frame_content ~state frame =
    let module Json = Yojson.Safe.Util in
    let open Opcode in
    let op = Json.(member "op" frame |> to_int) |> Opcode.from_int in
    match op with
    | DISPATCH -> dispatch ~payload:frame state
    (* | HEARTBEAT -> heartbeat shard
       | INVALID_SESSION -> begin
           Logs.err (fun m -> m "Invalid Session on Shard [%d, %d]: %s" (fst shard.id) (snd shard.id) (Yojson.Safe.pretty_to_string f));
           if J.(member "d" f |> to_bool) then
               initialize shard
           else begin
               initialize { shard with session = None; }
           end
       end
       | RECONNECT -> initialize shard *)
    | HELLO ->
        let hello = Json.member "d" frame |> Events.hello_of_yojson_exn in
        let heartbeat_interval = Some (float_of_int hello.heartbeat_interval) in
        let state = State.{ state with heartbeat_interval } in
        Lwt.return state
    | HEARTBEAT_ACK ->
        Logs.debug (fun m -> m "Received HEARTBEAT_ACK");
        Lwt.return state
    | opcode ->
        Logs.warn (fun m -> m "Invalid Opcode: %s" (Opcode.to_string opcode));
        Lwt.return state

  let client uri token =
    Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
    Conduit_lwt_unix.(endp_to_client ~ctx:default_ctx endp) >>= fun client ->
    Websocket_lwt_unix.with_connection ~ctx:Conduit_lwt_unix.default_ctx client
      uri
    >>= fun (recv, send) ->
    (* Wrap send with logging *)
    let send (frame : Frame.t) =
      Logs.debug (fun m -> m "Sending frame: %s" frame.content);
      send frame
    in
    (* Handle one frame *)
    let react frame =
      match frame with
      | `Beat state -> Lwt.return state
      | `Received (frame, state) -> (
          let open Websocket.Frame in
          match frame.opcode with
          | Opcode.Ping ->
              send @@ Frame.create ~opcode:Opcode.Pong () >|= fun () -> state
          | Opcode.Close ->
              (* Immediately echo and pass this last message to the user *)
              ( if String.length frame.content >= 2 then
                send
                @@ Frame.create ~opcode:Opcode.Close
                     ~content:(String.sub frame.content 0 2)
                     ()
              else send @@ Frame.close 1000 )
              >>= fun () -> Lwt.fail Exit
          | Opcode.Pong -> Lwt.return state
          | Opcode.Text | Opcode.Binary ->
              Yojson.Safe.from_string frame.content
              |> handle_frame_content ~state
          | _ -> send @@ Frame.close 1002 >>= fun () -> Lwt.fail Exit )
    in
    (* recv must forward the state *)
    let receive ~state () =
      recv () >>= fun (frame : Frame.t) ->
      Logs.debug (fun m ->
          m "Frame received op:%s content:%s"
            (Frame.Opcode.to_string frame.opcode)
            frame.content);
      Lwt.return (`Received (frame, state))
    in
    (* Automatic heartbeat *)
    let rec beat ~state () =
      let sleep_time = Option.value ~default:0. state.heartbeat_interval in
      Logs.debug (fun m -> m "Next heartbeat in %fms" sleep_time);
      Lwt_unix.sleep (sleep_time /. 1000.) >>= fun () ->
      let cmd = Commands.beat state.sequence_number in
      let content = Yojson.Safe.to_string cmd in
      Logs.debug (fun m -> m "Sent heartbeat: %s" content);
      send (Frame.create ~content ()) >>= beat ~state
      (* TODO: If a client does not receive a heartbeat ack between its attempts
         at sending heartbeats, it should immediately terminate the connection with a
         non-1000 close code, reconnect, and attempt to resume. *)
    in
    (* Loop on each received frame *)
    let rec react_forever state =
      receive ~state () >>= react >>= react_forever
    in
    let initialize token =
      let state = State.initial in
      let identify ~state token =
        (* Send Identify and receive Ready*)
        let cmd = Commands.identify token in
        let content = Yojson.Safe.to_string cmd in
        send (Frame.create ~content ()) >>= fun () ->
        receive ~state () >>= react >>= fun state ->
        if Option.is_none state.session_id then
          failwith "TODO no sessionid received"
        else Lwt.return state
      in

      (* First frame for discord should be HELLO *)
      receive ~state () >>= react >>= fun state ->
      if Option.is_none state.heartbeat_interval then
        failwith "TODO no hb received"
      else
        identify ~state token >>= fun state ->
        Lwt.pick [ beat ~state (); react_forever state ]
    in
    initialize token

  let start token =
    Http.Client.token := Some token;
    client (Uri.of_string "https://gateway.discord.gg/?v=6&encoding=json") token
end
