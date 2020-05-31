open Discord
open Lwt.Infix

let re =
  Re.(
    compile
      (whole_string
        (seq [ str "!bananarebours"; rep1 blank; group ( rep1 digit ) ])))

let make_text minutes =
  if minutes > 1 then
    List.init minutes (fun _ -> ":banana:") |> String.concat " "
  else "@everyone DRRRING"

let rec next channel message minutes =
  if minutes > 0 then
    let minutes = minutes - 1 in
    let payload =
    Rest.Edit_message.
      { content = make_text minutes; nonce = None; tts = false } in
    Lwt_unix.sleep 60.
      >>= fun () ->
        Rest.Edit_message.run ~payload channel message
      >>= fun _ -> next channel message minutes
  else Lwt.return_unit

let start channel minutes =
  let payload = Rest.Create_message.
  { content =  make_text minutes; nonce = None; tts = false } in
  if String.length payload.content < 2000 then
    Lwt.async (fun () -> Rest.Create_message.run ~payload channel >>= fun mess -> next mess.channel_id mess.id minutes)


let check ~cache:_ message =
  let open Model.Message in
  let res = Re.exec_opt re message.content in
  let res =
    let open Compat.Option.Infix in
    res >>= (fun g ->
        if Re.Group.test g 1 then Some (Re.Group.get g 1) else None)
        >>= int_of_string_opt

  in Option.iter (start message.channel_id) res