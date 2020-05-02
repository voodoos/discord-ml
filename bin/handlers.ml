open Lwt.Infix
open Model.Cache
include Gateway.Handlers.Default

let on_message ~cache message =
  let open Model.Message in
  Logs.debug (fun m ->
      m "I am a handler doing SOMETHING with the message %s" message.content);

  let find c txt =
    let upc = char_of_int (int_of_char c - 32) in
    String.index_opt txt c
    |> Option.map (fun i ->
           let byts = Bytes.of_string txt in
           Bytes.set byts i upc;
           Bytes.to_string byts)
  in

  if not (Int64.equal message.author.id cache.user.id) then
    let txt = String.lowercase_ascii message.content in

    let bind f o = Option.bind o f in

    let res =
      find 'p' txt
      |> bind (find 'o')
      |> bind (find 'u')
      |> bind (find 'e')
      |> bind (find 't')
      |> Option.map (fun s ->
             let payload = { Model.Actions.Create_message.content = s } in
             Model.Actions.Create_message.run ~payload message.channel_id
             >>= fun _ -> Lwt.return cache)
    in

    match res with None -> Lwt.return cache | Some lwt -> lwt
  else Lwt.return cache
