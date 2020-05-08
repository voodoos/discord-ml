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
