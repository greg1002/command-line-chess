open Pieces

let print_loc loc =
  print_string "(";
  fst loc |> string_of_int |> print_string;
  print_string ",";
  snd loc |> string_of_int |> print_string;
  print_string ") "; loc

let print_locs locs =
  print_string "Locs: ";
  List.map print_loc locs |> (fun _ -> ()); locs

let print_length l =
  print_string "Length: ";
  List.length l |> string_of_int |> print_string;
  print_string " "; l

let print_tile tile =
  match tile with
  | OwnedPiece (p,c) -> (
      print_string "(";
      (match c with
       | White -> "White"
       | Black -> "Black";
      ) |> print_string;
      (match p with
       | Queen -> " Queen"
       | King -> " King"
       | Knight -> " Knight"
       | Bishop -> " Bishop"
       | Rook -> " Rook"
       | Pawn -> " Pawn"
      ) |> print_string;
      print_string ") "; tile
    );
  | _ -> tile

let print_tiles tiles =
  print_string "Tiles: ";
  let new_tiles = List.map print_tile tiles in
  print_endline ""; new_tiles
