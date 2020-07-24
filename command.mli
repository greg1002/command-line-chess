(**
   Provides a command type representings possible actions in chess and functions
   to translate between command strings and that command type
*)

open Pieces

(** [loc] represents a location on the chess board*)
type loc = int * int
(** [stuff] represents any additional parameter that a chess action can have,
    include specifying taking, checking, checkmating, promoting, start location,
    and start column (only for pawns)*)
type stuff =
  | Take
  | Check
  | Checkmate
  | Promote of piece
  | Start of loc
  | PawnStart of int

(** [command] represents any possible action to be made in chess, with
      additional commands [Resign] and [Help] *)
type command = 
  | Move of piece * loc * stuff list
  | KingsideCastle of stuff list
  | QueensideCastle of stuff list
  | Resign
  | Help
  | History


(** [Malformed] is an [exception] raised whenever a string is
    is syntactically incorrect *)
exception Malformed of string
(** [Empty] is an [exception] raised whenver a string is empty*)
exception Empty
(** [Impossible] is an [exception] that should never be raised. *)
exception Impossible

(** [string_of_command c] returns the string representation of command [c]*)
val string_of_command : command -> string

(** [parse str] translates string [str] to type [command], representing a
    possible action in chess, accepting both algebraic and long-algebraic
    chess notation, as well as commands "help", "quit", and "resign". Throws an
    error if [str] is an syntactically invalid command. *)
val parse : string -> command