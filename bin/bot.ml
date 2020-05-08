open Discord

let () =
  let module Config : Config.S = struct
    let token = Sys.getenv "BOT_TOKEN"
  end in
  let module Client = Client.Make (Config) (Handlers) in
  Logging.setup Logs.Debug;
  Lwt_main.run (Client.start ())
