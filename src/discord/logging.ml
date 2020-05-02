let setup lvl =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some lvl)
