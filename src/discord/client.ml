module Make (Config : Config.S) (Handlers : Gateway.Handlers.S) = struct
  module GatewayCLient = Gateway.Client.Make (Handlers)

  let start () = GatewayCLient.start Config.token
end
