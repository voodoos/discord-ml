(** Internal Opcode abstractions. *)

(** From disml *)

(** Type of known opcodes. *)
type t =
  | DISPATCH
  | HEARTBEAT
  | IDENTIFY
  | STATUS_UPDATE
  | VOICE_STATE_UPDATE
  | RESUME
  | RECONNECT
  | REQUEST_GUILD_MEMBERS
  | INVALID_SESSION
  | HELLO
  | HEARTBEAT_ACK

exception Invalid_Opcode of int
(** Raised when receiving an invalid opcode. This should never occur. *)

val to_int : t -> int
(** Converts an opcode to its integer form for outgoing frames. *)

val from_int : int -> t
(** Converts an integer to an opcode for incoming frames.
    Raise {!Invalid_Opcode} Raised when an unkown opcode is received.
*)

val to_string : t -> string
(** Converts and opcode to a human-readable string. Used for logging purposes. *)
