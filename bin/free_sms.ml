open Discord
open Lwt.Infix

exception SMS_pass_not_found

let pass = try
  Sys.getenv "SMS_PASS"
with Not_found -> raise SMS_pass_not_found

let send ~user ~pass message =
  let endpoint =
    String.concat ""
      [
        "https://smsapi.free-mobile.fr/sendmsg?user=";
        string_of_int user;
        "&pass=";
        Uri.pct_encode pass;
        "&msg=";
        Uri.pct_encode message;
      ]
  in
  Cohttp_lwt_unix.Client.get (Uri.of_string endpoint)

let get_numbers () =
  let open Model.Message in
  let open Rest.Get_channel_messages in
  let payload = { limit = Some 50 } in
  run ~payload (Int64.of_string "708311030677110854") >>= fun messages ->
  let re =
    Re.(
      compile
        (whole_string
           (group
              (seq [ str "0"; alt [ str "6"; str "7" ]; repn digit 8 (Some 8) ]))))
  in
  List.filter_map
    (fun mess ->
      let res = Re.exec_opt re mess.content in
      let res =
        Option.bind res (fun g ->
            if Re.Group.test g 1 then Some (Re.Group.get g 1) else None)
      in
      res)
    messages
  |> Lwt.return
