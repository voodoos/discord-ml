module Cache = Model.Cache

module type S = sig
  type 'a handler = cache:Cache.t -> 'a -> Cache.t Lwt.t

  val on_message : Model.Message.t handler
end

module Default : S = struct
  type 'a handler = cache:Cache.t -> 'a -> Cache.t Lwt.t

  let on_message ~cache message =
    let open Model.Message in
    Logs.debug (fun m ->
        m "I am a handler doing nothing with the message %s" message.content);
    Lwt.return cache
end
