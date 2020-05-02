include Stdlib.Result

let iter_log ~ok = fold ~ok ~error:(fun s -> Logs.err (fun m -> m "%s" s))
