(** 
   The main entry point for the game interface.
*)

open Command
open State
open Pieces

(** [interp s com] is a router function that interprets command 
    [com] in state [s] by calling the respective interp commands*)
val interp : State.t -> command -> State.t

(** [print_board state] prints the board described by [get_board state]*)
val print_board : State.t -> unit

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit