open! Compat
open Lwt.Infix
open Model.Cache
include Gateway.Handlers.Default
module List = ListLabels

let sms_api_pass = Sys.getenv "SMS_PASS"

let sms_send = Free_sms.send ~user:17189984 ~pass:sms_api_pass

let on_message ~cache message =
  let open Model.Message in
  (* If message starts with !ping_all ping everyone in notifs channel *)
  let re = Re.(compile (whole_string (str "!ping_all"))) in
  if Re.execp re message.content then
    Lwt.async (fun () ->
        Free_sms.get_numbers () >>= fun numbers ->
        let number = String.concat ":" numbers in
        let message = Printf.sprintf "send:%s" number in
        sms_send message >|= ignore);

  if not (Int64.equal message.author.id cache.user.id) then
    let chars = [ 'p'; 'o'; 'u'; 'e'; 't' ] in
    let txt = String.lowercase_ascii message.content in
    let txt = Bytes.of_string txt in

    let up_first c prev =
      Bytes.index_from_opt txt prev c
      |> Option.map (fun i ->
             Bytes.set txt i (Char.uppercase_ascii c);
             i)
    in

    let res =
      List.fold_left chars ~init:(Some 0) ~f:(fun prev c ->
          Option.bind prev (up_first c))
    in

    if Option.is_some res then (
      let payload =
        Model.Actions.Create_message.
          { content = Bytes.to_string txt; nonce = None; tts = false }
      in
      Model.Actions.Create_message.run ~payload message.channel_id
      >>= fun mess ->
      let timebomb () =
        Lwt_unix.sleep 5. >>= fun () ->
        ( Logs.debug (fun m -> m "Deleting message %s" (Int64.to_string mess.id));
          Model.Actions.Delete_message.run mess.channel_id mess.id )
        >|= fun _ -> ()
      in
      Lwt.async timebomb;
      Lwt.return cache )
    else Lwt.return cache
  else Lwt.return cache
