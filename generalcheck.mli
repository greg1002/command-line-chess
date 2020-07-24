(** 
   Provides methods that insure the validity of any boardstate.
*)

open Pieces
open Command
open Board

(** [possible_moves h loc b] returns a list of int*int pairs representing the 
 * list of reachable tiles from the starting point [loc], depending on the piece 
 * occupying [loc] on board [b] and the list of 
 * previous moves [h]
*)
val possible_moves: command list -> loc -> Board.t -> loc list

(** [check_castle c h b is_queenside] returns board [b] if a castling move is
    valid for player of color [c] given board [c] and history [h], else it
    throws the approperiate error. If [is_queenside] is true, the castling move
    evaluated is a queenside castle, else a kingside castle.*)
val check_castle: color -> (color * command) list -> Board.t ->
  bool -> Board.t

(** [check_stuff h sl c b] returns [b] if stuff list [sl] correctly specifies
    the move by player of color [c] would put the opposing king in check
    or checkmante, given command history [h] and board [b]. Else it throws the
    approperiate error. *)
val check_stuff: color -> (color * command) list -> stuff list ->
  Board.t -> Board.t

(** [position_is_legal h c b] returns [b] if the position for player of
    color [c] is legal given command history [h] and board [b], meaning that
    that player's own king isn't in check. Else it throws the approperiate
    error. *)
val position_is_legal: color -> (color * command) list -> Board.t -> Board.t

(** [check_take h sl loc c b] returns [b] if stuff list [sl] correctly specifies
    the move by player of color [c] would take an opposing piece given
    command history [h] and board [b], else throwing the approperiate error.*)
val check_take: color -> loc -> (color * command) list -> stuff list
  -> Board.t -> Board.t

(** [check_promote sl loc c b] returns [None] if no promotion is possible
    for player of color [c] given stuff list [sl] and board [b], else returning
    [Some p], where [p] is the piece to promote to according to [sl]*)
val check_promote: color -> loc -> stuff list -> Board.t -> Pieces.piece option

(** [to_op_initloc_finalloc h intiloc board final_loc] is a transformation that
    turns a list of final locations [final_loc] for a piece that started at 
    [intiloc] at board [board] into a list of tuples of the form 
    (OwnedPiece, initial loc:loc, final loc:loc).
    Note: that this method filters out moves in final_loc that would result in
    ally king being in check *)
val to_op_initloc_finalloc: command list -> int*int -> Board.t ->
  (int*int) list -> ((tile * loc * loc) list)

(** [get_all_possible_moves h board c] is all the possible moves that player
    of color [c] can do in board [board]. Computes by calling possible_moves.
    This includes enpassant but does not include casteling*)
val get_all_possible_moves: command list -> Board.t -> color -> 
  (tile * loc * loc) list

(** [is_stalemate h c b] returns true if the player of color [color] is not 
    in check but has no legal moves (a stalemate) and false if this is not the
    case.
*)
val is_stalemate: (color* command) list -> color -> Board.t -> bool