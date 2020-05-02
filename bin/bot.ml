open Discord
module Client = Gateway.Client.Make (Handlers)

let () =
  (* let body = Lwt_main.run (Http.Client.get_gateway_bot ()) in
     print_endline ("Received body\n" ^ body) *)
  (* Logs_lwt.set_level (Some Logs.Info); *)
  Logging.setup Logs.Debug;
  let token = Sys.getenv "BOT_TOKEN" in
  Lwt_main.run (Client.start token)
