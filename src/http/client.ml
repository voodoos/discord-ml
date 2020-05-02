open Lwt
open Cohttp
open Cohttp_lwt_unix

type commands = Get of Endpoints.t | Post of Endpoints.t * Yojson.Safe.t

let url endp = Uri.of_string ("https://discordapp.com/api/v6" ^ endp)

let headers =
  Header.add_list (Header.init ())
    [
      ("User-Agent", "DiscordBot (https://github.com/voodoos, v0.0.1)");
      ( "Authorization",
        "Bot NzA0MzUyNDYwMDk3MDYwOTcz.XqgFHg.P3yAoOCheLfP-8DGciW8pKSsxho" );
      ("Content-Type", "application/json");
      ("Connection", "keep-alive");
    ]

let handle_response (resp, body) =
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let request command =
  ( match command with
  | Get endp -> Client.get ~headers (url endp)
  | Post (endp, payload) ->
      let body = `String (Yojson.Safe.to_string payload) in
      Client.post ~headers ~body (url endp) )
  >>= handle_response

let get_gateway_bot () = request (Get Endpoints.gateway_bot)
