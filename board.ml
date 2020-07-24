open Pieces
open Debug

module BoardCoords =
struct
  type t = int * int
  let compare x y =
    match compare (snd x) (snd y) with
      0 -> (compare (fst x) (fst y))
    | c -> -c
end;;


module B = Map.Make(BoardCoords)

let empty = B.empty

(** Ex: OwnedPiece (Pawn,black,(1,1)) or Empty (3,8)*)
(** Actually makes way more sense to use a map as we are mapping 
    a square to a piece. Also the Map contains alot of built in methods. 
    There is also a way to iterate over the map in asc order keys. This will be
    useful in printing. How the keys will be ordered is given by BoardCoords's
    compare function (which can be changed to make printing easier if necessary)
    https://ocaml.org/learn/tutorials/map.html
    https://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.Make.html
*)
(** 
    8     BLACK SIDE
    7
    6
    5
    4
    3
    2      WHITE SIDE 
    1
     1 2 3 4 5 6 7 8
    (a b c d e f g h)  
*)
type t = tile B.t

let form_coords l1 l2=
  List.fold_left (fun acc x -> 
      List.fold_left (fun ac h -> (x,h)::ac) acc l2
    ) [] l1

let get_inti_piece (c,r) =
  match c,r with
  | _,2 -> OwnedPiece (Pawn, White ) 
  | _,7 -> OwnedPiece (Pawn, Black )
  | 1,1 -> OwnedPiece (Rook, White)
  | 2,1 -> OwnedPiece (Knight, White)
  | 3,1 -> OwnedPiece (Bishop, White)
  | 4,1 -> OwnedPiece (Queen, White)
  | 5,1 -> OwnedPiece (King, White)
  | 6,1 -> OwnedPiece (Bishop, White)
  | 7,1 -> OwnedPiece (Knight, White)
  | 8,1 -> OwnedPiece (Rook, White)
  | 1,8 -> OwnedPiece (Rook, Black)
  | 2,8 -> OwnedPiece (Knight, Black)
  | 3,8 -> OwnedPiece (Bishop, Black)
  | 4,8 -> OwnedPiece (Queen, Black)
  | 5,8 -> OwnedPiece (King, Black)
  | 6,8 -> OwnedPiece (Bishop, Black)
  | 7,8 -> OwnedPiece (Knight, Black)
  | 8,8 -> OwnedPiece (Rook, Black)
  | _ -> EmptyTile

let get_rand_piece (c,r) rand_ls =
  let f x = List.nth rand_ls (x-1) in
  match c, r with
  | _,2 -> OwnedPiece (Pawn, White ) 
  | _,7 -> OwnedPiece (Pawn, Black )
  | c,1 when c=f 1 -> OwnedPiece (Rook, White)
  | c,1 when c=f 2 -> OwnedPiece (Knight, White)
  | c,1 when c=f 3 -> OwnedPiece (Bishop, White)
  | c,1 when c=f 4 -> OwnedPiece (Queen, White)
  | c,1 when c=f 5 -> OwnedPiece (King, White)
  | c,1 when c=f 6 -> OwnedPiece (Bishop, White)
  | c,1 when c=f 7 -> OwnedPiece (Knight, White)
  | c,1 when c=f 8 -> OwnedPiece (Rook, White)
  | c,8 when c=f 1 -> OwnedPiece (Rook, Black)
  | c,8 when c=f 2 -> OwnedPiece (Knight, Black)
  | c,8 when c=f 3 -> OwnedPiece (Bishop, Black)
  | c,8 when c=f 4 -> OwnedPiece (Queen, Black)
  | c,8 when c=f 5 -> OwnedPiece (King, Black)
  | c,8 when c=f 6 -> OwnedPiece (Bishop, Black)
  | c,8 when c=f 7 -> OwnedPiece (Knight, Black)
  | c,8 when c=f 8 -> OwnedPiece (Rook, Black)
  | _ -> EmptyTile

let init_board =
  let one_eight = [1;2;3;4;5;6;7;8] in 
  let all_coords = form_coords one_eight one_eight in 
  List.fold_left 
    (fun acc (c,r) -> B.add (c,r) (get_inti_piece (c,r)) acc) B.empty all_coords

let rand_board =
  let one_eight = [1;2;3;4;5;6;7;8] in 
  let all_coords = form_coords one_eight one_eight in 
  let rand_ls =
    [1;2;3;4;5;6;7;8] |>
    List.map (fun e -> (Random.bits (), e)) |>
    List.sort compare |>
    List.map snd in
  List.fold_left (fun acc (c,r) -> 
      B.add (c,r) (get_rand_piece (c,r) rand_ls) acc
    ) B.empty all_coords

let empty_board =
  let one_eight = [1;2;3;4;5;6;7;8] in 
  let all_coords = form_coords one_eight one_eight in 
  List.fold_left (fun acc (c,r) -> 
      B.add (c,r) EmptyTile acc
    ) B.empty all_coords

let get_piece (c,r) b =
  B.find (c,r) b 

let add loc t b =
  B.remove loc b |> B.add loc t

let remove loc b =
  B.remove loc b |> B.add loc EmptyTile 

let get_color (c,r) b =
  match B.find (c,r) b with
  | OwnedPiece (p,c) -> c
  | _ -> failwith "expected piece at c,r"

let move_piece iloc floc b =
  let op = B.find iloc b in
  remove iloc b |> add floc op 

let locations p c b =
  B.fold (fun k t l -> 
      match t with 
      | OwnedPiece(p_,c_) when p_ = p && c_ = c -> k::l
      |_ -> l
    ) b []

let locations_of_color c b =
  B.fold (fun k t l -> 
      match t with 
      | OwnedPiece(_,c_) when c = c_ -> k::l
      |_ -> l
    ) b []

let find_pawn_by_column col c b =
  B.fold (fun k t l ->
      match k with
      | (column,_) when col = column -> (
          match t with
          | OwnedPiece(Pawn, c_) when c = c_ -> k::l
          | _ -> l
        )
      | _ -> l
    ) b []

let find =
  B.find_opt

let get_iter=
  B.iter
