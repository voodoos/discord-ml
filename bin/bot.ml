open Discord

let () =
  let module Config : Config.S = struct
    let token = try
      Sys.getenv "BOT_TOKEN"
    with Not_found -> raise Config.Token_not_found
  end in
  let module Client = Client.Make (Config) (Handlers) in
  Logging.setup Logs.Debug;
  Lwt_main.run (Client.start ())
