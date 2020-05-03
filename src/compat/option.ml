include Stdlib.Option

module Infix = struct
  let ( >>= ) t f = bind t f

  let ( >|= ) t f = map f t
end
