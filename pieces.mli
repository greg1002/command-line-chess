(** 
   Provides necessary types to represent the pieces and colors on a chess board.
*)

(** [piece] represents any possible piece in chess *)
type piece = Pawn |
             King |
             Queen |
             Rook |
             Knight |
             Bishop 

(** [color] represents the possible color of the pieces or players in chess *)
type color = White | Black

(** [tile] represents the possible content of a tile in the chess board *)
type tile = EmptyTile | OwnedPiece of piece * color

(** [opp_color c] returns the opposing color to color [c] *)
val opp_color : color -> color