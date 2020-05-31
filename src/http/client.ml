open Lwt
open Cohttp
open Cohttp_lwt_unix

type commands =
  | Get of Uri.t
  | Delete of Endpoints.t
  | Post of Endpoints.t * Yojson.Safe.t
  | Patch of Endpoints.t * Yojson.Safe.t

let token = ref None (* TODO improve token handling *)

let headers () =
  Header.add_list (Header.init ())
    [
      ("User-Agent", "DiscordBot (https://github.com/, v0.0.1)");
      ("Authorization", "Bot " ^ Option.value ~default:"" !token);
      ("Content-Type", "application/json");
      ("Connection", "keep-alive");
    ]

let handle_response (resp, body) =
  let code = resp |> Response.status |> Code.code_of_status in
  Logs.debug (fun m -> m "Response code: %d\n" code);
  Logs.debug (fun m ->
      m "Headers: %s\n" (resp |> Response.headers |> Header.to_string));
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Logs.debug (fun m ->
      m "Body of length: %d\n<<<%s>>>\n" (String.length body) body);
  body

let request command =
  let headers = headers () in
  ( match command with
  | Get uri -> Client.get ~headers uri
  | Delete endp -> Client.delete ~headers (Endpoints.to_uri endp)
  | Post (endp, payload) ->
      Logs.debug (fun m -> m "Sending: %s" (Yojson.Safe.to_string payload));
      let body = `String (Yojson.Safe.to_string payload) in
      Client.post ~headers ~body (Endpoints.to_uri endp)
  | Patch (endp, payload) ->
    Logs.debug (fun m -> m "Sending: %s" (Yojson.Safe.to_string payload));
    let body = `String (Yojson.Safe.to_string payload) in
    Client.patch ~headers ~body (Endpoints.to_uri endp) )
  >>= handle_response

let get_gateway_bot () = request (Get (Uri.of_string Endpoints.gateway_bot))
