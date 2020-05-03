open! Compat
open Lwt.Infix
open Model.Cache
include Gateway.Handlers.Default
module List = ListLabels

let on_message ~cache message =
  let open Model.Message in
  Logs.debug (fun m ->
      m "I am a handler doing SOMETHING with the message %s" message.content);

  if not (Int64.equal message.author.id cache.user.id) then
    let chars = [ 'p'; 'o'; 'u'; 'e'; 't' ] in
    let txt = String.lowercase_ascii message.content in

    let txt = Bytes.of_string txt in
    let up_first c () =
      Bytes.index_opt txt c
      |> Option.map (fun i -> Bytes.set txt i (Char.uppercase_ascii c))
    in

    let res =
      List.fold_left chars ~init:(Some ()) ~f:(fun prev c ->
          Option.bind prev (up_first c))
    in

    if Option.is_some res then
      let payload =
        { Model.Actions.Create_message.content = Bytes.to_string txt }
      in
      Model.Actions.Create_message.run ~payload message.channel_id >>= fun _ ->
      Lwt.return cache
    else Lwt.return cache
  else Lwt.return cache
