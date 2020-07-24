open Pieces
open Command
open Board
open Debug

(** [string_of_char c] returns the string corresponding to char [c]*)
let string_of_char c = String.make 1 c

(** [locs (c,r) b color range (hd,vd)] returns a list of int*int pairs 
    corresponding to the tiles on the chess board [b] reachable from starting 
    point [(c,r)] moving in the horizontal direction [hd] and vertical 
    direction [vd] with a maximum range of [range]
*)
let rec locs (c,r) b color range (hd,vd) =
  match (find (c+hd,r+vd) b) with
  | _ when range = 0 -> []
  | Some(EmptyTile) -> (c+hd,r+vd)::locs (c+hd,r+vd) b color (range-1) (hd,vd) 
  | Some(OwnedPiece (_,clr)) when clr <> color -> [(c+hd,r+vd)]
  | _ -> []
(** [knight_locs (c,r) b color] returns a list of int*int pairs representing 
    the tiles reachable by a knight of color [color] starting from location 
    [(c,r)] on board [b]
*)
let knight_locs (c,r) b color =
  let is_open (c_,r_) =
    match (find (c_,r_) b) with
    | Some(EmptyTile) -> Some (c_,r_)
    | Some(OwnedPiece (_,clr)) when clr <> color -> Some (c_,r_)
    | _ -> None
  in 
  List.filter_map is_open 
    [(c+2,r+1);(c+2,r-1);(c-2,r+1);(c-2,r-1);
     (c+1,r+2);(c+1,r-2);(c-1,r+2);(c-1,r-2)]
(** [en_passant (c,r) color history b] returns [Some loc] if a pawn of color 
    [color] attacking the location [(c,r)] can take out the piece at loc using 
    the en passant maneuver. Returns [None] if this is not possible
*)
let en_passant (c,r) color (history: command list) b : (int * int) option=
  if history = [] then None else
    let valid_row,forbidden_row,opp_color = 
      match color with White -> 5,6,Black 
                     | Black -> 4,3,White in 
    match List.hd history with 
      Move(Pawn,(col,row),l) 
      when col=c && row = valid_row && not (List.mem Take l) ->
      (match (find (col,row) b) with Some(OwnedPiece(Pawn,clr)) 
        when clr = opp_color ->
        (let rec list_help = function 
            |Move(Pawn,(col,row),_)::t when col=c && row = forbidden_row ->
              false
            |h::t -> list_help t
            |[] -> true in if list_help (List.tl history) && r = forbidden_row 
         then Some(col,row) else None)
                                   | _-> None)
    |_->None

(** [pawn_locs h (c,r) b color] returns a list of int*int pairs representing 
    all reachable tiles for a pawn starting at [(c,r)] of color [color], taking 
    into account attacking nearby pawns and the possibility of an en passant
    maneuver.
*)
let pawn_locs (h:command list) (c,r) b color =

  let (+-?) = match color with
      White -> (+) | Black -> (-) in
  let left =
    (match find (c-1, r +-? 1) b with
     |Some(OwnedPiece(_,clr)) when clr <> color -> [(c-1, r +-? 1)]
     |Some(EmptyTile) when ((en_passant (c-1, r +-? 1) color h b) = 
                            Some(c-1, r)) -> [(c-1, r +-? 1)]
     |_-> []) in
  let right = 
    match find (c+1, r +-? 1) b with
    |Some(OwnedPiece(_,clr)) when clr <> color -> [(c+1, r +-? 1)]
    |Some(EmptyTile) when en_passant (c+1, r +-? 1) color h b = 
                          Some(c+1,r) -> [(c+1, r +-? 1)]
    |_->[] in
  let forward = 
    let starting_row =
      match color with
        Black -> 7 | White -> 2 in
    match (find (c,r +-? 1) b),(find (c, r +-? 2) b) with
    |Some(EmptyTile),Some(EmptyTile) when r = starting_row  -> 
      [(c, r +-? 1);(c,r +-? 2)]
    |Some(EmptyTile),_ -> [(c,r +-? 1)]
    |_->[] in
  left @ forward @ right


let possible_moves h loc b =
  match find loc b with
  |Some EmptyTile -> []
  |None -> failwith "Invalid"
  |Some OwnedPiece(p,c) -> 
    (match p with 
     | King -> 
       List.map (locs loc b c 1) 
         [(0,1);(1,0);(-1,0);(0,-1);(1,1);(1,-1);(-1,-1);(-1,1)] |> List.flatten
     | Queen -> 
       List.map (locs loc b c 8) 
         [(0,1);(1,0);(-1,0);(0,-1);(1,1);(1,-1);(-1,-1);(-1,1)] |> List.flatten
     | Rook -> 
       List.map (locs loc b c 8) [(0,1);(1,0);(-1,0);(0,-1)] |> List.flatten
     | Bishop -> 
       List.map (locs loc b c 8) [(1,1);(1,-1);(-1,-1);(-1,1)] |> List.flatten
     | Knight -> (knight_locs loc b c)
     | Pawn -> pawn_locs h loc b c) 

let poss_locs (h : command list) (c:color) (b: Board.t) =
  locations_of_color (opp_color c) b |>
  List.map (fun loc -> possible_moves h loc b) |>
  List.flatten |>
  List.sort_uniq (BoardCoords.compare)

let is_checked (h : command list) (c:color) (b: Board.t) =
  poss_locs h c b |>
  List.fold_left (fun is_checked loc ->
      if is_checked then true
      else let p = find loc b in
        p = Some (OwnedPiece(King, c))
    ) false


(** [to_op_initloc_finalloc h intiloc b final_loc] is a transformation that
    turns a list of final locations [final_loc] for a piece that started at 
    [intiloc] at board [b] into a list of tuples of the form 
    (OwnedPiece, initial loc:loc, final loc:loc).
    Note: that this method filters out moves in final_loc that would result in
    ally king being in check *)
let to_op_initloc_finalloc (h : command list) (intiloc:int*int) (b: Board.t) 
    (final_loc:(int*int) list): ((tile * loc * loc) list) =
  List.fold_left (fun acc (c,r) -> 
      let b' = Board.move_piece intiloc (c,r) b in
      (if is_checked h (get_color intiloc b) b'
       then acc else (get_piece intiloc b, intiloc, (c,r))::acc)
    ) [] final_loc

(** [get_all_possible_moves h b c] is all the possible moves that player
    of color [c] can do in board [b]. Computes by calling possible_moves.
    This includes enpassant but does not include casteling*)
let get_all_possible_moves (h :command list) (b:Board.t) (c:color) =
  let board = b in 
  List.fold_left (fun acc (c,r)-> 
      (to_op_initloc_finalloc h (c,r) board (possible_moves h (c,r) board))@acc)
    [] (locations_of_color c board)

(** [stuff_opt_add_take p c floc b] is "x" if piece [p]'s final location
    [floc] is inhabited by an enemy piece. "-" otherwise. 
    Note: [c] is ally color*)
let stuff_opt_add_take (p:piece) (c:color) (floc:int*int) (b:Board.t) =
  match get_piece floc b with
  | OwnedPiece (piece,color) when color<>c -> "x"
  | _ -> "-"

(** 1->a, 2->b etc...*)
(** [to_str i] is a maping that turns 1 into "a", 2 into "b" etc...*)
let to_str (i:int) =
  Char.chr (96+i) |> string_of_char

(** [build_piece_command p c iloc floc b] is the command that results in
    piece [p] of color [c] at initial location [iloc] moving to final location 
    [floc] on board [b]. Calls Command.parse to generate the Command.command
*)
let build_piece_command (p:piece) (c:color) (iloc: int *int) 
    (floc:int*int) (b:Board.t) : (Command.command) =
  (**[build_command p c iloc floc b piece_str] is the string that after 
     being passed to Command.parse will generate the appropriate command.
     Strings follow the pattern (piece char)(column char of iloc)
     (row num of iloc)(x or -)(column char of floc)(row num of floc) *)
  let build_command p c iloc floc b piece_str=
    piece_str^to_str (fst iloc) ^ string_of_int (snd iloc) ^ 
    stuff_opt_add_take p c floc b ^ to_str (fst floc) ^ 
    string_of_int (snd floc) |> Command.parse in
  (match p with
   | Pawn -> "" | King -> "K" | Queen -> "Q"
   | Bishop -> "B" | Rook -> "R" | Knight -> "N") |>
  build_command p c iloc floc b

(** [turn_pos_moves_to_command op_iloc_floc_list b] is the list of commands
    generated from [op_iloc_floc_list] (which is of the form 
    (OwnedPiece, initial loc:loc, final loc:loc) list). Calls 
    build_piece_command *)
let turn_pos_moves_to_command (op_iloc_floc_list:(tile * loc * loc) list) 
    (b: Board.t) : (command list) =
  List.fold_left (fun acc (tile,iloc,floc) -> 
      match tile with
      | OwnedPiece (p,c)-> 
        (build_piece_command p c iloc floc b)::acc
      | EmptyTile -> failwith "should not be empty") [] op_iloc_floc_list

(** [check_castle_arrangement c b is_queenside] returns unit if the arrangement
    of board [b] is valid for castling for player of color [c], else throwing
    an error.*)
let check_castle_arrangement (c:color) (b:Board.t) (is_queenside:bool) =
  (match c with
   | White -> 
     if is_queenside then
       [((1,1), OwnedPiece (Rook, White));((2,1), EmptyTile);((3,1), EmptyTile);
        ((4,1), EmptyTile); ((5,1), OwnedPiece (King, White));]
     else
       [((8,1), OwnedPiece (Rook, White));((7,1), EmptyTile);((6,1), EmptyTile);
        ((5,1), OwnedPiece (King, White));]
   | Black -> 
     if is_queenside then
       [((1,8), OwnedPiece (Rook, Black));((2,8), EmptyTile);((3,8), EmptyTile);
        ((4,8), EmptyTile); ((5,8), OwnedPiece (King, Black));]
     else
       [((8,8), OwnedPiece (Rook, Black));((7,8), EmptyTile);((6,8), EmptyTile);
        ((5,8), OwnedPiece (King, Black));]
  ) |>
  List.fold_left (
    fun _ m ->
      match m with | (loc, piece) ->
        if find loc b = Some piece then ()
        else failwith "Invalid board arrangement for castling"
  ) ()

(** [check_castle_passing c b is_queenside] returns unit if the king of
    color [c] wouldn't pass any checked squares while castling on board [b],
    else throwing an error.*)
let check_castle_passing (c:color) (b:Board.t) (is_queenside:bool) =
  get_all_possible_moves [] b (opp_color c) |>
  List.map (function | (_,_,loc) -> loc) |>
  fun locs ->
  locs |>
  ((match c with
      | White -> if is_queenside then (4,1) else (6,1)
      | Black -> if is_queenside then (4,8) else (6,8)) |>
   List.mem) |>
  (function
    | true ->
      failwith "King mustn't pass through squares attacked by enemy pieces"
    | false -> ());
  locs |>
  ((match c with | White -> (5,1) | Black -> (5,8)) |>
   List.mem) |>
  (function
    | true -> failwith "King mustn't castle out of check"
    | false -> ())

(** [check_castle_history c h is_queenside] returns unit if the king or the
    rook of color [c] hasn't moved yet according history [h],
    else throwing an error.*)
let check_castle_history c h is_queenside =
  List.iter (fun command ->
      match command with
      | (col, Move (King,_,_)) when
          col = c -> failwith "King musn't have moved to castle"
      | (col, Move (Rook,loc,_)) when
          col = c &&
          loc = (match col with
              | White -> (if is_queenside then (1,1) else (8,1))
              | Black -> (if is_queenside then (1,8) else (8,8)))
        -> failwith "Rook musn't have moved to castle"
      | _ -> ()
    ) h

let check_castle c h b is_queenside = 
  check_castle_arrangement c b is_queenside;
  check_castle_passing c b is_queenside;
  check_castle_history c h is_queenside;
  b

(** [can_kingside_castle h c b] returns [true] if kingside castling is 
    possible for player of color [c] given board [b] and command history [h],
    else [false]*)
let can_kingside_castle c h b=
  try (let _ = check_castle c h b false in true) with _ -> false

(** [can_queenside_castle h c b] returns [true] if queenside castling is 
    possible for player of color [c] given board [b] and command history [h],
    else [false]*)
let can_queenside_castle c h b=
  try (let _ = check_castle c h b true in true) with _ ->false

(** [get_possible_commands h b c] is a list of possible commands 
    that payer of color [c] can move from board [b] *)
let get_possible_commands (h:(color * command) list)
    (b: Board.t) (c:color) =
  let ret = ref [] in
  let op_iloc_floc_list = 
    get_all_possible_moves (List.map snd h) b in 
  let all_but_castle = 
    (turn_pos_moves_to_command (op_iloc_floc_list c) b) in 
  (** below handles casteling*)
  let () = ret := all_but_castle in 
  (let _ =
     if can_kingside_castle c h b then
       ret := ((Command.parse "0-0") :: !ret)
   in 
   if can_queenside_castle c h b then
     ret := ((Command.parse "0-0-0") :: !ret));
  !ret

(** [no_possible_moves h c b] returns [true] if there are no possible moves
    for the player of color [c] given command histroy [h] and board [b], else
    [false]. *)
let no_possible_moves (h:(color*command) list) (c:color) (b: Board.t) = 
  get_possible_commands h b c = []

(** [check_stuff h sl c b] returns [b] if stuff list [sl] correctly specifies
    the move by player of color [c] would put the opposing king in check
    or checkmante, given command history [h] and board [b]. Else it throws the
    approperiate error. *)
let check_stuff c h sl b  =
  let checked = is_checked (List.map snd h) (opp_color c) b in
  let checkmated = if checked then no_possible_moves h (opp_color c) b 
    else false in
  if checkmated then (
    if List.mem Checkmate sl then ()
    else failwith "Must indicate if move leads to checkmate"
  ) else if checked then (
    if List.mem Checkmate sl then failwith "Move doesn't lead to checkmate"
    else if List.mem Check sl then ()
    else failwith "Must indicate if move leads to check"
  )
  else if List.mem Check sl then failwith "Move doesn't lead to check"
  else ();
  b

(** [position_is_legal h c b] returns [b] if the position for player of
    color [c] is legal given command history [h] and board [b], meaning that
    that player's own king isn't in check. Else it throws the approperiate
    error. *)
let position_is_legal c h b =
  let commands = List.map snd h in
  if is_checked commands c b then 
    failwith "Own king mustn't be checked after move" else ();
  b
(** [check_take c loc h sl b] returns a valid board state [b] if the state of 
    the target tile [loc] agrees with the commands in [sl]. For example, if 
    [Take] is included and the target tile is occupied by a piece of the 
    opposite color of [c], [b] is returned. However, if [loc] is empty, 
    a failure occurs. If the target tile [loc] is empty and [Take] is specified,
    a valid state is only returned if an en_passant is possible. In this case, 
    the piece that is taken by the en passant is removed, and that board is 
    returned.
*)
let check_take c loc h sl b =
  let commands = List.map snd h in
  (match find loc b with
   | Some (OwnedPiece(_,color)) when color == opp_color c ->
     if List.mem Take sl then b else failwith ""
   | Some EmptyTile ->
     if List.mem Take sl then (
       match (en_passant loc c commands b) with
       | Some location -> (remove location b)
       | _-> (failwith "Invalid Move")
     ) else b
   | _ -> failwith "Impossible")

(** [check_promote c loc sl b] returns [Some p] if the pawn of color [c] at 
    location [loc] can be promoted to piece p as a result of the command in 
    [sl], and [None] otherwise.
*)
let check_promote c loc sl b =
  let rec mem_help = function 
    | Promote(p)::t -> Some(p)
    | h::t -> (mem_help t)
    | [] -> None in
  let opp_row = match c with 
    | White -> 8
    | Black -> 1 in
  match (mem_help sl) with 
  | Some(p) when p <> King && p <> Pawn &&
                 snd loc = opp_row &&
                 (find loc b) = Some (OwnedPiece(Pawn,c))
    -> Some(p)
  | None -> None
  | _ -> failwith "this promotion cannot be made"

let is_stalemate (h:(color*command) list) (c:color) (b: Board.t) =
  (no_possible_moves h c b) && not (is_checked (List.map snd h) c b)

