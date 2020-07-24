
type piece = Pawn |
             King |
             Queen |
             Rook |
             Knight |
             Bishop 

type color = White | Black

type tile = EmptyTile | OwnedPiece of piece * color

let opp_color c =
  match c with
  | Black -> White
  | White -> Black