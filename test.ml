open Command
open Board
open Generalcheck
open State
open Pieces
open OUnit2

(** Test Plan: 
    The nature of making a game like Chess neceisitates a fair a amount of 
    manual testing, but we made use of automatic testing with OUnit as well. 
    We manually tested things like the game loop and some of the moves and game 
    states, esentially just playing through a chess game, to ensure the user 
    experience was as intended. We also used glass-box testing during 
    development to confirm that certain functions were acheiving the desired 
    effect. For example, we tested the [possible_moves] function on all piece 
    types to verify that the list of reachable coordinates matched with the 
    rules of chess. This was to test the [Generalcheck] module. We also used 
    glass-box testing to test the [Command] module, making sure inputs were 
    parsed and processed into commands as expected. Additionally, we made use 
    of black-box testing on moves to test the [State] module, making sure that 
    Illegal moves fail and Legal ones succeed. This testing method demonstrates 
    the correctness of the system because it tests both general functionality 
    in the form of moves, commands, and the game loop as well as more specific 
    tests ranging from board-state specific possible move for each piece, and 
    niche scenarios such as en passant, check, and castling. 
    Each of these scenarios has very specific conditions that need
    to hold for the move to be valid, so we used extensive testing to 
    verify that they perform as expected.
*)

(********************************************************************
   HELPER METHODS FROM A2 RELEASE CODE 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [assert_true b] is the functional equivalent of [assert] *)
let assert_true b = assert b

(** [pp_string s] pretty-prints string [s]. *)
let pp_pair p = "(" ^ (p |> fst |> string_of_int) ^ ", "^ (p |> snd |> 
                                                           string_of_int) ^")"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* helper methods from a2 end*)
(* Helper boards for testing*)
let en_passant_board_white = empty_board |> add (1,5) (OwnedPiece(Pawn,White)) 
                             |> add (2,5) (OwnedPiece(Pawn,Black))
let en_passant_board_black = empty_board |> add (2,4) (OwnedPiece(Pawn,White)) 
                             |> add (1,4) (OwnedPiece(Pawn,Black))

let possible_move_tests = [
  "White Rook" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair) [] 
        (possible_moves [] (1,1) init_board) );
  "White Knight" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair)
        [(1,3);(3,3)] (possible_moves [] (2,1) init_board) );
  "White Pawn" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair)
        [(1,3);(1,4)] (possible_moves [] (1,2) init_board));
  "Black Pawn" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair)
        [(1,6);(1,5)] (possible_moves [] (1,7) init_board));
  "Black Rook" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair)
        [] (possible_moves [] (1,8) init_board) );
  "Black Knight" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair)
        [(1,6);(3,6)] (possible_moves [] (2,8) init_board) );
  "Bishop - Empty Board" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair)
        [(2,2);(3,3);(4,4);(5,5);(6,6);(7,7);(8,8)] 
        (possible_moves [] (1,1) (add (1,1) (OwnedPiece(Bishop,White)) 
                                    empty_board)));
  "Knight - Empty Board" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair)
        [(3,2);(2,3);(3,6);(2,5);(5,2);(6,3);(6,5);(5,6)] 
        (possible_moves [] (4,4)  (add (4,4) (OwnedPiece(Knight,White)) 
                                     empty_board)) );
  "King - Empty Board" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair)
        [(3,3);(3,4);(3,5);(4,5);(4,3);(5,3);(5,5);(5,4)] 
        (possible_moves [] (4,4)  (add (4,4) (OwnedPiece(King,White)) 
                                     empty_board)) );
  "Queen - Empty Board" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair)
        [(1,2);(1,3);(1,4);(1,5);(1,6);(1,7);(1,8);(2,1);(3,1)
        ;(4,1);(5,1);(6,1);(7,1);(8,1);(2,2);(3,3);(4,4);(5,5)
        ;(6,6);(7,7);(8,8)] 
        (possible_moves [] (1,1)  (add (1,1) (OwnedPiece(Queen,White)) 
                                     empty_board)) );
  "En Passant - White" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair)
        [(1,6);(2,6)] (possible_moves [(Move (Pawn,(2,5),[]))] (1,5)  
                         (en_passant_board_white)));
  "En Passant - Black" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair)
        [(1,3);(2,3)] (possible_moves [(Move (Pawn,(2,4),[]))] (1,4) 
                         (en_passant_board_black)));
  "En Passant - Fail" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_pair)
        [(1,6)] (possible_moves [(Move (Pawn,(2,5),[]));(Move (Pawn,(2,6),[]))]
                   (1,5) (en_passant_board_white)));
]

let black_init_state = def_state init_board [] Black

let move_tests = [
  "Black Pawn" >:: (fun _ -> 
      assert (match (move (Pawn,(4,5),[]) black_init_state) with Legal s -> 
          true | Illegal _ -> false ));
  "White Pawn" >:: (fun _ -> 
      assert (match (move (Pawn,(4,4),[]) init_state) with Legal s -> 
          true | Illegal _ -> false ));
  "Failed Double Step" >:: (fun _ -> 
      assert (match (move (Pawn,(4,3),[]) init_state) with Legal s -> 
          (match (move (Pawn,(4,5),[]) (def_state (get_board s) [] White)) 
           with Legal _ -> false | Illegal _ -> true )
                                                         |Illegal _ -> false));
  "Fail Bishop" >:: (fun _ -> 
      assert (match (move (Bishop,(4,4),[]) init_state) with Legal s -> 
          false | Illegal _ -> true));
  "Fail Knight" >:: (fun _ -> 
      assert (match (move (Knight,(4,4),[]) init_state) with Legal s -> 
          false | Illegal _ -> true));
  "Kingside Castle - White" >:: (fun _ -> 
      def_state 
        (empty_board |> add (5,1) (OwnedPiece(King,White)) |> 
         add (8,1) (OwnedPiece(Rook,White))) 
        [] White |>
      king_side_castle [] |>
      (function | Legal _ -> true |_->false)
      |> assert_true
    );
  "Queenside Castle - White" >:: (fun _ ->  
      def_state 
        (empty_board |> add (5,1) (OwnedPiece(King,White)) |> 
         add (1,1) (OwnedPiece(Rook,White))) 
        [] White |>
      queen_side_castle [] |>
      (function | Legal _ -> true |_->false)
      |> assert_true
    );
  "Kingside Castle - Black" >:: (fun _ -> 

      def_state 
        (empty_board |> add (5,8) (OwnedPiece(King,Black)) |> 
         add (8,8) (OwnedPiece(Rook,Black))) 
        [] Black |>
      king_side_castle [] |>
      (function | Legal _ -> true |_->false)
      |> assert_true
    );
  "Queenside Castle - Black" >:: (fun _ ->  
      def_state 
        (empty_board |> add (5,8) (OwnedPiece(King,Black)) |> 
         add (1,8) (OwnedPiece(Rook,Black))) 
        [] Black |>
      queen_side_castle [] |>
      (function | Legal _ -> true |_->false)
      |> assert_true
    );
  "Kingside Castle - Fail (Pressure)" >:: (fun _ -> 
      def_state 
        (empty_board |> add (5,1) (OwnedPiece(King,White)) |> 
         add (8,1) (OwnedPiece(Rook,White)) |> add (7,6) 
           (OwnedPiece(Rook,Black))) [] White |>
      king_side_castle [] |>
      (function | Legal _ -> false |_->true) |>
      assert_true
    );
  "Queenside Castle - Fail (Pressure)" >:: (fun _ ->  
      def_state 
        (empty_board |> add (5,1) (OwnedPiece(King,White)) |> 
         add (1,1) (OwnedPiece(Rook,White)) |> add (3,3) 
           (OwnedPiece(Knight,Black))) [] White |>
      queen_side_castle [] |>
      (function | Legal _ -> false |_->true) |>
      assert_true
    );
  "Kingside Castle - Fail (Move)" >:: (fun _ -> 

      def_state 
        (empty_board |> add (5,1) (OwnedPiece(King,White)) |> 
         add (8,1) (OwnedPiece(Rook,White)))
        [(White,Move(Rook,(8,1),[]))] White |>
      king_side_castle [] |>
      (function | Legal _ -> false |_->true) |>
      assert_true
    );
  "Queenside Castle - Fail (Move)" >:: (fun _ ->  
      def_state 
        (empty_board |> add (5,1) (OwnedPiece(King,White)) |> 
         add (1,1) (OwnedPiece(Rook,White))) 
        [(White,Move(King,(5,1),[]))] White |>
      queen_side_castle [] |>
      (function | Legal _ -> false |_->true) |>
      assert_true
    );
  "Promotion" >:: (fun _ -> 
      def_state (empty_board |> add (1,7) (OwnedPiece(Pawn,White))) [] White |>
      move (Pawn,(1,8),[Promote Queen]) |>
      (function | Legal _ -> true | Illegal _ -> false) |>
      assert_true
    );
  "Promotion - Fail1" >:: (fun _ -> 
      def_state (empty_board |> add (1,6) (OwnedPiece(Pawn,White))) [] White |>
      move (Pawn,(1,7),[Promote Queen]) |>
      (function | Legal _ -> false | Illegal _ -> true) |>
      assert_true
    );
  "Promotion - Fail2" >:: (fun _ -> 
      def_state (empty_board |> add (1,7) (OwnedPiece(Pawn,White))) [] White |>
      move (Pawn,(1,8),[Promote King]) |>
      (function | Legal _ -> false | Illegal _ -> true) |>
      assert_true
    );
]

let create_p_loc_loc p iloc floclist : (tile * loc * loc) list=
  List.fold_left (fun acc (c,r)-> (p,iloc,(c,r))::acc) [] floclist

let create_triple (iloc:loc) (floc_list:loc list) (p:piece) (c:color) =
  List.fold_left (fun acc x -> (OwnedPiece(p,c), iloc, x)::acc) [] floc_list

let rec make_pawn_loc_list c n acc=
  let init = if c =White then 2 else 7 in
  if n=0 then acc else
    make_pawn_loc_list c (n-1) ((n,init)::acc)

let get_ipawn_moves (cl:color) : (tile * loc * loc) list=
  let (+-?) = if cl =White then (+) else (-) in
  List.map (fun (c,r) -> (OwnedPiece (Pawn,cl),(c,r),(c,r+-?1))::
                         (OwnedPiece (Pawn,cl),(c,r),(c,r+-?2))::[] ) 
    (make_pawn_loc_list cl 8 []) |> List.flatten

let init_knight_move_locs c=
  match c with
  | White ->
    (create_p_loc_loc (OwnedPiece (Knight,White)) (2,1) [(1,3);(3,3)])@
    (create_p_loc_loc (OwnedPiece (Knight,White)) (7,1) [(8,3);(6,3)])
  | Black -> 
    (create_p_loc_loc (OwnedPiece (Knight,Black)) (2,8) [(1,6);(3,6)])@
    (create_p_loc_loc (OwnedPiece (Knight,Black)) (7,8) [(8,6);(6,6)])

let possible_command_tests = [
  "triple generation" >:: 
  (fun _ -> assert_equal ~cmp: cmp_set_like_lists 
      (to_op_initloc_finalloc [] (1,1) init_board [(2,2);(3,3);(1,2)] ) 
      (create_triple (1,1) [(2,2);(3,3);(1,2)] Rook White ) );

  "White init_board command check" >:: 
  (fun _ -> assert_equal ~cmp: cmp_set_like_lists 
      (get_all_possible_moves [] (init_state |> get_board) White) 
      (get_ipawn_moves White @ init_knight_move_locs White) );
  "Black init_board command check" >:: 
  (fun _ -> assert_equal ~cmp: cmp_set_like_lists 
      (get_all_possible_moves [] (init_state |> get_board) Black) 
      (get_ipawn_moves Black @ init_knight_move_locs Black) );
]


let game_mode_tests = [
  "Chess" >:: (fun _ -> ignore init_state);
  "Random" >:: (fun _ -> ignore rand_state);
]

let various_function_tests = [
  "Pawn by column" >:: (fun _ -> 
      assert_equal ~cmp: cmp_set_like_lists ~printer: 
        (pp_list pp_pair) [(4,3);(4,4)] 
        (find_pawn_by_column 4 White (
            empty_board |> add (4,4) (OwnedPiece(Pawn,White)) |>
            add (4,3) (OwnedPiece(Pawn,White))
          )));
  "Check test" >:: (fun _ -> 
      check_stuff White [] [Check] (
        empty_board |> add (4,4) (OwnedPiece(Rook, White)) |>
        add (4,8) (OwnedPiece(King,Black))
      ) |> ignore);
  "Check take" >:: (fun _ ->
      check_take White (4,4) [] [Take] (
        empty_board |> add (2,4) (OwnedPiece(Rook, White)) |> 
        add (4,4) (OwnedPiece(Rook, Black))
      ) |> ignore
    );
  "Check take - Fail" >:: (fun _ ->
      (try
         (check_take White (4,7) [] [Take] (
             empty_board |> add (2,4) (OwnedPiece(Rook, White)) |> 
             add (4,8) (OwnedPiece(Rook, Black)))
         ) |> ignore; false
       with Failure _ -> true) |>
      assert_true
    );
  "Stalemate" >:: (fun _ ->
      is_stalemate [] White (empty_board |> add (1,1) (OwnedPiece(King,White))
                             |> add (3,2) (OwnedPiece(Queen,Black))) 
      |> assert_true
    );
  "Not Stalemate" >:: (fun _ ->
      is_stalemate [] White init_board 
      |> not |> assert_true
    );
]

let command_tests = [
  "Command Pawn Move" >:: (fun _ ->
      assert_equal (Move (Pawn, (1,4), [PawnStart 1])) (parse "a4"));
  "Command Pawn Take" >:: (fun _ ->
      assert_equal (Move (Pawn, (5,6), [PawnStart 4; Take])) (parse "dxe6"));
  "Command Pawn Move (long algebraic)" >:: (fun _ ->
      assert_equal (Move (Pawn, (3,4), [Start (3,2)])) (parse "c2-c4"));
  "Command Pawn Take (long algebraic)" >:: (fun _ ->
      assert_equal (Move (Pawn, (7,5), [Start (8,4); Take])) (parse "h4xg5"));
  "Command Pawn Promote (long algebraic)" >:: (fun _ ->
      assert_equal (Move (Pawn, (1,8), [PawnStart 1; Promote Knight])) 
        (parse "a8=N"));
  "Command Pawn everything" >:: (fun _ ->
      assert_equal (Move (Pawn, (3,8), [Start (2,7); Take; Promote Queen; 
                                        Checkmate])) (parse "b7xc8=Q#"));
  "Command Piece Move" >:: (fun _ ->
      assert_equal (Move (Knight, (3,3), [])) (parse "Nc3"));
  "Command Piece Take" >:: (fun _ ->
      assert_equal (Move (Queen, (8,8), [Take; Check])) (parse "Qxh8+"));
  "Command Piece Move (long algebraic)" >:: (fun _ ->
      assert_equal (Move (Bishop, (6,4), [Start (3,1)])) (parse "Bc1-f4"));
  "Command Piece Take (long algebraic)" >:: (fun _ ->
      assert_equal (Move (Rook, (5,8), [Start(5,1); Take; Checkmate])) 
        (parse "Re1xe8#"));
  "Command Queenside Castling" >:: (fun _ ->
      assert_equal (QueensideCastle [Check]) (parse "0-0-0+"));
  "Command Kingside Castling" >:: (fun _ ->
      assert_equal (KingsideCastle [Checkmate]) (parse "0-0#"));
]

let tests = [possible_move_tests;command_tests;move_tests;
             possible_command_tests;game_mode_tests;various_function_tests]

let suite = "search test suite" >::: List.flatten tests

let _ = run_test_tt_main suite