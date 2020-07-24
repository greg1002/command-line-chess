(** 
    Provides debug methods to insure the functionality of any module using
    module [Pieces].
*)

open Pieces

(** [print_loc loc] prints the string representation of location [loc] and
    returns [loc]*)
val print_loc : int * int -> int * int

(** [print_loc locs] prints the string representation of location list
    [locs] and returns [locs]*)
val print_locs : (int * int) list -> (int * int) list

(** [print_length l] prints the length of list [l] and
    returns [l]*)
val print_length : 'a list -> 'a list

(** [print_tile tile] prints the string representation of tile [tile] and
    returns [tile]*)
val print_tile : tile -> tile

(** [print_tiles tiles] prints the string representation of tile list
    [tiles] and returns [tiles]*)
val print_tiles : tile list -> tile list
