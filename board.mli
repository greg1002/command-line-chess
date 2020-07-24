(**
    Provides the representation type for the chess board and functions to
    interact with it.
*)

open Pieces
open Debug

(** [t] is the type that represents the chess board *)
type t

(** [BoardCoords] is the module where [BoardCoords.t] represents the key
    to retrieve any tile on the chessboard*)
module BoardCoords : sig
  type t
  val compare: int * int -> int * int -> int
end

(** [init_board] is a chess board at the start of a standard chess match *)
val init_board: t

(** [rand_board] is a chess board with a randomized back row on both sides*)
val rand_board: t

(** [emptyBoard] is a chess board with no pieces *)
val empty_board: t

(** [get_piece (c,r) b] returns an [OwnedPiece(p,color)] if 
 * the tile (c,r) is occupied, and [EmptyTile] if not.
 * Raises [Not_Found] if (c,r) are not valid chess board coordinates.
*)
val get_piece: int * int -> t -> tile

(** [add loc t b] returns the board that results from adding tile [t]
    at [loc]*)
val add: int * int -> tile -> t -> t

(** [remove loc t b] returns the board that results from removing the tile
    at [loc] and replacing it with [EmptyTile]*)
val remove: int * int -> t -> t

(** [get_color loc b] returns the color of the piece at location [loc] given
    board [b]. Raises an error if there is no piece at [loc]*)
val get_color: int * int -> t -> color

(* [move_piece iloc floc b] removes the piece at location [iloc] and adds it
    to location [floc] given board [b]*)
val move_piece: int * int -> int * int -> t -> t

(** [locations p c b] returns a list of all locations on board [b] that
    contain an [OwnedPiece] of piece type [p] and color [c]*)
val locations: piece -> color -> t -> (int * int) list

(** [locations_of_color c b] returns a list of all locations on 
    [b] that contain a piece of color [c]*)
val locations_of_color: color -> t -> (int * int) list

(** [find_pawn_by_column col c b] returns a list of all locations in column 
    [col] of board [b] containing Pawns of color [c]*)
val find_pawn_by_column: int -> color -> t -> (int * int) list

(** [find loc b] returns [Some(OwnedPiece)] if [loc] is occupied on [b],
    [Some(EmptyTile)] if [loc] is empty, and [None] if [loc] are invalid 
    coordinates*)
val find: int * int -> t -> tile option

(**[iter f b] is the [iter] function of [Map] *)
val get_iter: (int * int -> tile -> unit) -> t -> unit