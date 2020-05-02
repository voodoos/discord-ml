open Lwt.Infix
open Model.Cache
include Gateway.Handlers.Default

module List = ListLabels

let on_message ~cache message =
  let open Model.Message in
  Logs.debug (fun m ->
      m "I am a handler doing SOMETHING with the message %s" message.content);

  if not (Int64.equal message.author.id cache.user.id) then
    let txt = String.lowercase_ascii message.content in

  let txt = Bytes.of_string txt in
  let chars = ['p';'o';'u';'e';'t'] in
  if (List.for_all ~f:(fun c -> Bytes.contains txt c) chars) then
    (List.iter chars
      ~f:(fun c -> Bytes.(set txt (index txt c) (Char.uppercase_ascii c)));

    let payload = { Model.Actions.Create_message.content = Bytes.to_string txt } in
    Model.Actions.Create_message.run ~payload message.channel_id
  >>= fun _ -> Lwt.return cache)
   else Lwt.return cache

  else Lwt.return cache
