open Command
open Board
open Generalcheck
open Pieces


type t = { 
  board: Board.t; 
  history: (color * command) list;
  turn: color;
  is_over: bool;
}

type result = Legal of t | Illegal of string

let init_state =
  {
    board = init_board;
    history = [];
    turn= White;
    is_over = false;
  }

let rand_state = 
  {
    board = rand_board;
    history = [];
    turn= White;
    is_over = false;
  }

let def_state b h t =
  {
    board = b;
    history = h;
    turn= t;
    is_over = false;
  }

let get_is_over s =
  s.is_over

let get_turn s =
  s.turn

let get_board s =
  s.board

let get_history s =
  s.history

let color_text s =
  match s.turn with
  | White -> "white"
  | Black -> "Black"

(**
   [find_Start p move sl s] returns the first possible origin of piece [p] based
   on parameters move-to location [move], stuff list [sl], and state [s]. Throws
   an error if no start location can be found.
*)
let rec find_start (p:piece) (move:int*int) (sl:stuff list) (s:t) : loc =
  let rec helper poss_starts = 
    match poss_starts with
    | [] -> failwith "Move can't be made"
    | loc::t ->
      Generalcheck.possible_moves (List.map snd s.history) loc s.board 
      |> List.mem move |> function | true -> loc | false -> helper t in
  match sl with
  | [] -> (
      locations p s.turn s.board |> helper
    )
  | h :: t ->
    match h with
    | PawnStart c -> (
        if p = Pawn then () else failwith "Piece must be a pawn";
        find_pawn_by_column c s.turn s.board |> helper
      )
    | Start loc -> (
        [loc] |> helper
      )
    | _ -> find_start p move t s

(** [promote sl loc c b] returns a new board with Piece [p] of color 
    [c] at [loc] in [board] if a promotion to [p] is specified in the 
    command [sl] and this promotion is possible. If not, an unchanged board \
    [board] is returned.
*)
let promote (sl:stuff list) (loc:int*int) (c:color) (b:Board.t) : Board.t =
  match (check_promote c loc sl b) with
  | Some p -> add loc (OwnedPiece(p, c)) b
  | None -> b

let move (p, loc, sl) s =
  try 
    let start = find_start p loc sl s in
    let board = 
      check_take s.turn loc s.history sl s.board |>
      remove start |> add loc (OwnedPiece (p, s.turn)) |> 
      promote sl loc s.turn |>
      check_stuff s.turn s.history sl |>
      position_is_legal s.turn s.history  in
    Legal {
      board = board;
      history = (s.turn, Move (p, loc, Start start::sl)) :: s.history;
      turn = opp_color s.turn;
      is_over = List.mem Checkmate sl;
    }
  with | Failure msg -> Illegal msg

let king_side_castle sl s =
  try
    check_castle s.turn s.history s.board false |>
    ((match s.turn with
        | White ->
          [remove (8,1); add (6,1) (OwnedPiece (Rook, White));
           remove (5,1); add (7,1) (OwnedPiece (King, White))]
        | Black ->
          [remove (8,8); add (6,8) (OwnedPiece (Rook, Black));
           remove (5,8); add (7,8) (OwnedPiece (King, Black))]) |>
     List.fold_right (fun f b -> f b)) |>
    check_stuff s.turn s.history sl |>
    position_is_legal s.turn s.history |>
    fun b -> Legal {
      board = b;
      history = (s.turn, KingsideCastle sl) :: s.history;
      turn = opp_color s.turn;
      is_over = List.mem Checkmate sl;
    }
  with | Failure msg -> Illegal msg

let queen_side_castle sl s =
  try
    check_castle s.turn s.history s.board true |>
    ((match s.turn with
        | White ->
          [remove (1,1); add (4,1) (OwnedPiece (Rook, White));
           remove (5,1); add (3,1) (OwnedPiece (King, White))]
        | Black ->
          [remove (1,8); add (4,8) (OwnedPiece (Rook, Black));
           remove (5,8); add (3,8) (OwnedPiece (King, Black))]) |>
     List.fold_right (fun f b -> f b)) |>
    check_stuff s.turn s.history sl |>
    position_is_legal s.turn s.history  |>
    fun b -> Legal {
      board = b;
      history = (s.turn, QueensideCastle sl) :: s.history;
      turn = opp_color s.turn;
      is_over = List.mem Checkmate sl;
    }
  with | Failure msg -> Illegal msg

let resign s =
  Legal {
    board = s.board;
    history = s.history;
    turn = opp_color s.turn;
    is_over = true
  }