type t = {
  session_id : string option;
  heartbeat_interval : float option;
  cache : Model.Cache.t option;
  mutable sequence_number : int option;
}

let initial =
  {
    session_id = None;
    heartbeat_interval = None;
    cache = None;
    sequence_number = None;
  }

let get_cache_exn s = Option.get s.cache
