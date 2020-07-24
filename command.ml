open Pieces

type loc = int * int

type stuff =
  | Take
  | Check
  | Checkmate
  | Promote of piece
  | Start of loc
  | PawnStart of int

type command = 
  | Move of piece * loc * stuff list
  | KingsideCastle of stuff list
  | QueensideCastle of stuff list
  | Resign
  | Help
  | History

exception Malformed of string
exception Empty
exception Impossible

(** [string_of_char c] returns the string corresponding to char [c]*)
let string_of_char c = String.make 1 c

let piece_chars = ['N';'R';'B';'Q';'K']
(** [is_p chr] returns whether [chr] represents a piece*)
let is_p chr = List.mem chr piece_chars
(** [to_p chr] returns the piece of type [piece] corresponding to [chr]*)
let to_p = function | 'N' -> Knight | 'R' -> Rook | 'B' -> Bishop
                    | 'Q' -> Queen | 'K' -> King | _ -> raise Impossible
(** [p_to_string p] returns the string representation of piece [p]*)
let p_to_string p =
  match p with
  | Pawn -> ""
  | Rook -> "R"
  | Knight -> "N"
  | King -> "K"
  | Queen -> "Q"
  | Bishop -> "B"

(** [col_chars] is the list of possible chars representing columns*)
let col_chars = ['a';'b';'c';'d';'e';'f';'g';'h']
(** [is_c] returns whether [c] represents a column *)
let is_c chr = List.mem chr col_chars
(** [c_to_chr c] returns the string representation of column [c]*)
let c_to_chr c = List.nth col_chars (c - 1)

(** [row_chars] is the list of possible chars representing rows*)
let row_chars = ['1';'2';'3';'4';'5';'6';'7';'8']
(** [is_r] returns whether [r] represents a row *)
let is_r chr = List.mem chr row_chars
(** [r_to_chr r] returns the string representation of row [r]*)
let r_to_chr r = List.nth row_chars (r - 1)

(** [to_int chr] returns the integer corresponding to [chr]*)
let to_int = function
  | chr when is_c chr -> Char.code chr - 96
  | chr when is_r chr -> Char.code chr - 48
  | _ -> raise Impossible

(** [is_loc] returns whether [c] and [r] represent a valid location *)
let is_loc c r = is_c c && is_r r
(** [to_loc c r] returns the location of type [loc] corresponding to [c]
    and [r]*)
let to_loc c r = (to_int c, to_int r)
(** [loc_to_string c r] returns the string representation of [(c,r)]*)
let loc_to_string (c, r) = 
  (c_to_chr c |> string_of_char) ^ (r_to_chr r |> string_of_char)

(** [string_of_stuff pr sf sl] returns the string representation of the command
    with prefix [pr] and suffix [sf] with the string representation of
    everything in [sl] added on*)
let string_of_stuff pr sf sl =
  let rec has_start_helper sl =
    match sl with
    | Start (_, _) :: t -> true
    | h :: t -> has_start_helper t
    | [] -> false in
  let has_start = has_start_helper sl in
  let rec helper pr sf sl = 
    match sl with
    | [] -> pr ^ sf
    | h :: t ->
      match h with
      | Promote p -> helper pr (sf ^ "=" ^ p_to_string p) t
      | Checkmate -> helper pr sf t ^ "#"
      | Check -> helper pr sf t ^ "+"
      | Take -> helper pr ("x" ^ sf) t
      | PawnStart c ->
        helper (pr ^ if has_start then  ""
                else (c_to_chr c |> string_of_char)) sf t
      | Start (c,r) ->
        helper (pr ^ loc_to_string (c, r) ^ (
            if has_start then "-" else "" )) sf t in
  helper pr sf sl

let string_of_command c =
  match c with
  | QueensideCastle sl -> string_of_stuff "" "0-0-0" sl
  | KingsideCastle sl -> string_of_stuff "" "0-0" sl
  | Move (p, loc, sl) ->
    string_of_stuff (p_to_string p) (loc_to_string loc) sl
  | _ -> failwith "Command has no corresponding chess notation"

(* [explode str] converts string [str] to type [char list] *)
let explode str =
  let rec explode_inner cur_index chars = 
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [new_char])
    else chars in
  explode_inner 0 []

(* [explode str] converts char list [chars] to type [string] *)
let rec implode chars =
  match chars with
    [] -> ""
  | h::t ->  string_of_char h ^ (implode t)

(** [parse_end chars] parses the suffixes in a move represented by char list
    [chars], returning a list of type [stuff list] representing all parsed
    tokens *)
let rec parse_end = function
  | '+'::t -> Check::parse_end t
  | '#'::t -> Checkmate::parse_end t
  | [] -> []
  | x -> raise (Malformed ("Unexpected suffix '" ^ implode x ^ "'"))

(** [parse_end_pawns chars] parses the suffixes in a move represented by char
    list [chars], returning a list of type [stuff list] representing all parsed
    tokens. Invariant: [chars] must represent notation for a pawn move. Because
    of this, it additionally can parse commands involving piece promotions.  *)
let rec parse_end_pawn = function
  | '='::p::t when is_p p-> Promote(to_p p)::parse_end t
  | t -> parse_end t

(** [parse_notation str] translates string [str] to type [command], representing
    a possible action in chess, accepting only algebraic and long-algebraic
    chess notation. Throws an error if [str] is an syntactically invalid
    command. *)
let parse_notation str = 
  match explode str with
  | _c::_r::t when is_loc _c _r -> (match t with
      | '-'::c::r::t when is_loc c r ->
        Move (Pawn, to_loc c r, Start (to_loc _c _r)::parse_end_pawn t)
      | 'x'::c::r::t when is_loc c r ->
        Move (Pawn, to_loc c r, Start(to_loc _c _r)::Take::parse_end_pawn t)
      | t -> Move (Pawn, to_loc _c _r, PawnStart (to_int _c)::parse_end_pawn t))
  | _c::'x'::c::r::t when is_c _c && is_loc c r ->
    Move (Pawn, to_loc c r, PawnStart (to_int _c)::Take::parse_end_pawn t)
  | p::_c::_r::t when is_p p && is_loc _c _r -> (match t with
      | '-'::c::r::t when is_loc c r ->
        Move (to_p p, to_loc c r, Start(to_loc _c _r)::parse_end t)
      | 'x'::c::r::t when is_loc c r ->
        Move (to_p p, to_loc c r, Start(to_loc _c _r)::Take::parse_end t)
      | t -> Move (to_p p, to_loc _c _r, parse_end t))
  | p::'x'::c::r::t when is_p p && is_loc c r ->
    Move (to_p p, to_loc c r, Take::parse_end t)
  | '0'::'-'::'0'::t -> (match t with
      | '-'::'0'::t -> QueensideCastle (parse_end t)
      | t -> KingsideCastle (parse_end t))
  | _ -> raise (Malformed ("Unkown command '" ^ str ^ "'"))

let parse str =
  match str with
  | "help" -> Help
  | "quit"
  | "resign" -> Resign
  | "history" -> History
  | str -> 
    let str_array = String.split_on_char ' ' str in 
    (match str_array with
     | [] -> raise Empty
     | [x] -> ()
     | h :: t ->
       raise (Malformed ("Unexpected '" ^ String.concat " " t ^ "'")));
    parse_notation str