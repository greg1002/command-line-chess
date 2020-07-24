open Command
open State
open Pieces
open Generalcheck

(** encodes which game mode is selected*)
type gamemode = Normal | Random

(**[print text color] is a helper function that uses ANSITerminal to print 
   [text] with color [color] *)
let print text color =
  ANSITerminal.(print_string [color] text)

(** The current ascii charcter to use for border*)
let border_literal = "\xe2\x96\x92"

(** [get_color_text c] is "White" if [c]=White else "Black"*)
let get_color_text c =
  match c with 
  | Black -> "Black"
  | White ->"White"

(** [get_chess_literal p c] is the string that encodes chess peiece [p] 
    with color [c].
    Note: include opp_color for black terminals*)
let get_chess_literal p c =
  match p,opp_color c with 
  | King, Black -> "\xe2\x99\x9a"
  | King, White -> "\xe2\x99\x94"
  | Queen, Black -> "\xe2\x99\x9b"
  | Queen, White ->  "\xe2\x99\x95"
  | Pawn, Black ->  "\xe2\x99\x9f"
  | Pawn, White -> "\xe2\x99\x99"
  | Knight, Black -> "\xe2\x99\x9e"
  | Knight, White -> "\xe2\x99\x98"
  | Bishop, Black -> "\xe2\x99\x9d"
  | Bishop, White -> "\xe2\x99\x97"
  | Rook, Black -> "\xe2\x99\x9c"
  | Rook, White -> "\xe2\x99\x96"

(** [get_printable_string (c,r) b] is the string that describes piece at 
    [(c,r)] in board [b] with the border_literal and necessary spaces.*)
let get_printable_string (c,r) b=
  match Board.find (c,r) b with 
  | Some op -> 
    (match op with               (**| numbering goes here for nums*)
     | OwnedPiece (piece,color) ->
       border_literal^" "^ 
       " " ^ get_chess_literal piece color ^ " "
     (**| numbering goes here for nums*)                
     | EmptyTile -> border_literal ^ "    ")
  | None -> failwith "impossible"

(** [print_el_or_not bool (c,r) b] calls [get_printable_string (c,r) b] and
    adds a "\n" if [bool]=true*)
let print_el_or_not bool (c,r) b=
  if bool then 
    print_string ((get_printable_string (c,r) b)^ " " ^ border_literal ^ "\n" )
  else
    print_string ((get_printable_string (c,r) b)^ " "  )

(** [print_x_times str x] prints string [str] [x] times and addds newline "\n" 
    at end*)
let rec print_x_times str x=
  if x=1 then print_string "\n" 
  else let () = print_string str in print_x_times str (x-1)

let print_board s =
  let b =get_board s in
  let counter = ref 0 in let first = ref true in 
  let () =
    Board.get_iter (fun (c,r) v-> 
        let () =  if !counter =0 then 
            (** ensures that first row does not have | | | above*)
            let () = (** 1st row*)if !first then first :=false 
              else (let () = first:=false in 
                    print_x_times (border_literal ^ "     ") 10) in
            let () = (** 2nd row*)print_x_times border_literal 50 in
            print_x_times (**3rd row*)(border_literal ^ "     ") 10 

          else () in
        let () = if !counter=7 then print_el_or_not true (c,r) b 
          else print_el_or_not false (c,r) b in 
        if !counter=7 then counter := 0 else counter := !counter+1 )

      b in let () = (** one extra 1st row in second to last row*)
             print_x_times (border_literal ^ "     ") 10 
  in let () =print_x_times border_literal 50
  in  print_string ("\n"^ (get_turn s |> get_color_text) ^ "\'s turn\n\n> ") 

(** [help_text] is help information for how to play chess*)
let help_text =
  "HOW TO PLAY:
  To make a move, use enter commands to move using algebraic or long algebraic
  chess notation. Notation syntax can be found at
  'https://en.wikipedia.org/wiki/Algebraic_notation_(chess)'. Type \"resign\" if
  you want to resign and type \"history\" if you want to see the move 
  history. \n\n\n"

(** [interp_move (p, loc,  sl) s] interprets the move command 
    encoded by [((p, loc,  sl))] in state [s].*)
let interp_move (p, loc,  sl) s=
  match move (p,loc,sl) s with 
  | Legal s -> s
  | Illegal str -> print (str^"\n") ANSITerminal.red; s

(** [interp_ksc sl s] interprets the king side castle
    command encoded by [((p, loc,  sl))] in state [s].*)
let interp_ksc sl s =
  match king_side_castle sl s with
  | Legal s -> s
  | Illegal str-> print (str^"\n") ANSITerminal.red; s

(** [interp_qsc sl s] interprets the queen side castle
    command encoded by [((p, loc, sl))] in state [s].*)
let interp_qsc sl s =
  match queen_side_castle sl s with
  | Legal s -> s
  | Illegal str-> print (str^"\n") ANSITerminal.red; s

(** [interp_resign s] interprets the resign command in state [s]
    and prints out a message for the winner*)
let interp_resign s=
  match resign s with
  | Legal s -> let winner = color_text s in 
    print (winner ^ " wins\n\n") ANSITerminal.green; exit 0
  | Illegal _-> print "impossible" ANSITerminal.red; exit 0

(** [interp_history s] interprets the history command in state [s]
    and prints out the history of moves*)
let interp_history s =
  print "White Moves " ANSITerminal.yellow;
  print "Black Moves " ANSITerminal.blue;
  get_history s |>
  List.rev |>
  List.iter (fun (color, command) ->
      (match color with
       | Black -> ANSITerminal.blue
       | White -> ANSITerminal.yellow) |>
      print (string_of_command command ^ "  ")
    );
  print_string "\n\n"


(** [is_over s] prints a win message for the winner of game described by 
    [s] and exits if game has ended. If game has not ended then simply 
    returns the s*)
let is_over s =
  if get_is_over s = true then 
    (let _ = print_board s;
       let other_color = s |> get_turn |> opp_color in
       print_string (get_color_text other_color ^ " wins\n\n") in
     exit 0)
  else s

let interp s com =
  match com with
  | Move (p, loc,  stuff_list) -> interp_move (p, loc,  stuff_list) s
  | KingsideCastle stuff_list ->interp_ksc stuff_list s
  | QueensideCastle stuff_list -> interp_qsc stuff_list s
  | Resign -> interp_resign s
  | Help -> print help_text ANSITerminal.green; s
  | History -> interp_history s; s

let check_stalemate s=
  if is_stalemate (get_history s) (get_turn s) (get_board s) then 
    (let () = print_string "stalemate\n" in exit 0)
  else s
(** [game_loop s] is a game loop that repeatedly asks user to provide a 
    command and then parses, interprets the command *)
let rec game_loop s= 
  let ()= print_board s in
  try
    match read_line () with
    | exception End_of_file -> ()
    | command -> (**after droping ceck if game ended*)
      parse command |> interp s |> is_over |> check_stalemate |> 
      game_loop 
  with Command.Malformed(e) 
    -> print ("Error: " ^ e^"\n") ANSITerminal.red;
    game_loop s

(** [play_game mode] is the game of type [mode]. Calls [game_loop] *)
let play_game (mode :gamemode) =
  match mode with 
  | Normal -> game_loop init_state
  | Random -> game_loop rand_state

let main () =
  ANSITerminal.(print_string [green] "\n\nChess\n");
  print_string  "Enter \"Chess\" to play normal chess, and \"Random\" 
  to play random chess\n";
  match read_line () with
  | exception End_of_file -> ()
  | "Random" -> play_game Random
  | "Chess" -> play_game Normal
  | _ -> ()


(* Executes the game engine.*)
let () = main ()
