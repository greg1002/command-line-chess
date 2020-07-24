(**
   Provides a state type representing any state of a chess board and functions
   to interact with the state.
*)

open Command
open Board
open Generalcheck
open Pieces

(** [t] represents the current state of any game. For any [s : t], board
    [s.board] would represent the layout of the chess board, [s.history] would
    represent the sequence of commands commited so far, color [s.turn] would
    represent the color whose player is in turn, and boolean [s.is_over] would
    represent whether the game is over or not.*)
type t

(** [result] represents the result of a player executing commands on a
    current state. [Legal s] is returned if the command is valid, where [s]
    would be the resulting state. [Illegal] is returned if the command is
    invalid.
*)
type result = Legal of t | Illegal of string

(** [init_state] returns the initial state of a standard chess game*)
val init_state : t

(** [rand_state] returns the initial state of a chess game with randomized
    backrows on the board*)
val rand_state : t

(** [def_state b h t] returns a state based on parameters board [b], history [h]
    and color [t]*)
val def_state : Board.t -> (color * command) list -> color  -> t

(** [get_is_over s] returns boolean [s.is_over] *)
val get_is_over : t -> bool

(** [get_turn s] returns color [s.turn] *)
val get_turn : t -> color

(** [get_board s] returns board [s.board] *)
val get_board : t -> Board.t

(** [get_history s] returns history [s.history] *)
val get_history : t -> (color * command) list

(** [color_text s] returns the string representation of the color whose player
    is in turn according to state [s]*)
val color_text : t -> string

(** [move (p, loc, sl) s] returns a [Legal ns] result if the state [ns] 
    created by moving piece [p] to location [loc] with stuff list [sl]
    as parameters give state [s] is a valid chess state,
    and [Illegal] otherwise.
*)
val move : piece * loc * stuff list -> t -> result

(** [king_side_castle sl s] returns a [Legal ns] result if the state [ns] 
    created kingside castling with stuff list [sl] as parameters given state [s]
    is a valid chess state, and [Illegal] otherwise.
*)
val king_side_castle : stuff list -> t -> result

(** [queen_side_castle sl s] returns a [Legal ns] result if the state [ns] 
    created queenside castling with stuff list [sl] as parameters given state 
    [s] is a valid chess state, and [Illegal] otherwise.
*)
val queen_side_castle : stuff list -> t -> result

(** [resign s] returns a [Legal nx] result where the state [ns] is the result
    of the player in turn resiging given state [s]
*)
val resign : t -> result