type t = {
  p1_enemy : Board.t;
  p1_self : Board.t;
  p2_enemy : Board.t;
  p2_self : Board.t;
  p1_self_sunk : int;
  p2_self_sunk : int;
}

type result = Legal of t | Illegal

let init_state =
  {
    p1_enemy = Board.empty_guess_board;
    p1_self = Board.empty_ship_board;
    p2_enemy = Board.empty_guess_board;
    p2_self = Board.empty_ship_board;
    p1_self_sunk = 0;
    p2_self_sunk = 0;
  }

let p1_enemy_board st =
  st.p1_enemy

let p1_player_board st =
  st.p1_self

let p2_enemy_board st =
  st.p2_enemy

let p2_player_board st =
  st.p2_self

let p1_self_sunk st = 
  st.p1_self_sunk

let p2_self_sunk st = 
  st.p2_self_sunk

let get_p1_score st =
  st.p2_self_sunk

let get_p2_score st = 
  st.p1_self_sunk


(* -----------Place functions---------------------- *)


(** [list_helper_chr chr range start] creates the position list if the ship
    is vertical *)
let rec list_helper_chr num range start= 
  if range > 0 
  then 
    (Char.chr (range + start - 1), num)::list_helper_chr num (range - 1) start
  else [] 

(** [list_helper_int chr range start] creates the position list if the ship
    is horizontal *)
let rec list_helper_int (chr:char) range start= 
  if range > 0 
  then (chr, range+start - 1)::list_helper_int chr (range - 1) start
  else []

(** [pair_to_list pair] is a list of (char*int) that is the list of 
    positions between the positions in [pair]*)
let pair_to_list pair =
  let chr1 = fst (fst pair) in 
  let chr2 = fst (snd pair) in 
  let num1 = snd (fst pair) in 
  let num2 = snd (snd pair) in
  if (chr1 = chr2) 
  then list_helper_int chr1 (num2 - num1 + 1) num1
  else let chr_range = ((Char.code chr2) - (Char.code chr1) + 1) in 
    list_helper_chr num1 chr_range (Char.code chr1)

(** [place_helper lst brd] places all ships in [lst] onto ship board [brd]*)
let rec place_helper lst brd size=
  match lst with 
  | h::t -> place_helper t (Board.place_ship brd h size) size
  | [] -> brd


let p1_place ship_places st =
  let list1 = pair_to_list ship_places in 
  let size = List.length list1 in 
  if Board.check_placement st.p1_self size ship_places list1 then
    let m_temp = place_helper (list1) st.p1_self size in 
    Legal {
      p1_enemy = st.p1_enemy;
      p1_self = Board.set_length_list m_temp (List.length list1); 
      p2_enemy = st.p2_enemy;
      p2_self = st.p2_self;
      p1_self_sunk = st.p1_self_sunk;
      p2_self_sunk = st.p2_self_sunk;
    }
  else
    Illegal

let p2_place ship_places st =
  let list1 = pair_to_list ship_places in 
  let size = List.length list1 in 
  if Board.check_placement st.p2_self size ship_places list1 then
    let m_temp = place_helper list1 st.p2_self size in 
    Legal {
      p1_enemy = st.p1_enemy;
      p1_self = st.p1_self;
      p2_enemy = st.p2_enemy;
      p2_self = Board.set_length_list m_temp (List.length list1); 
      p1_self_sunk = st.p1_self_sunk;
      p2_self_sunk = st.p2_self_sunk;
    }
  else
    Illegal



(* -----------Guess functions---------------------- *)

let on_board spot = 
  let a = (Char.code (fst spot) >= 65) && ( Char.code (fst spot) <= 74) in 
  let f = (snd spot >= 1 && snd spot <= 10) in 
  a && f

let p1_guess spt st = 
  if ((List.length spt) = 1 && on_board (List.hd spt) && 
      not (Board.get_shot st.p1_enemy (List.hd spt))) 
  then 
    let spot = List.hd spt in 
    let list_of_ships = Board.get_placed_ships st.p2_self in 
    if List.exists (fun x -> x = spot) list_of_ships then 
      (print_endline "Hit!"; 
       let new_p2_self = Board.place_hit st.p2_self spot in
       Legal {
         p1_enemy = Board.place_hit st.p1_enemy spot;
         p1_self = st.p1_self;
         p2_enemy = st.p2_enemy;
         p2_self = new_p2_self;
         p1_self_sunk = st.p1_self_sunk;
         p2_self_sunk = if Board.is_ship_sunk new_p2_self spot then 
             (st.p2_self_sunk + 1) else st.p2_self_sunk; }
      ) 
    else (print_endline "Miss!"; 
          Legal {
            p1_enemy = Board.place_miss st.p1_enemy spot;
            p1_self = st.p1_self;
            p2_enemy = st.p2_enemy;
            p2_self = Board.place_miss st.p2_self spot; 
            p1_self_sunk = st.p1_self_sunk;
            p2_self_sunk = st.p2_self_sunk;}
         ) 
  else Illegal



let p2_guess spt st = 
  if ((List.length spt) = 1 && on_board (List.hd spt) && 
      not (Board.get_shot st.p2_enemy (List.hd spt)))
  then 
    let spot = List.hd spt in 
    let list_of_ships = Board.get_placed_ships st.p1_self in 
    if List.exists (fun x -> x = spot) list_of_ships then 
      let new_p1_self = Board.place_hit st.p1_self spot in
      (print_endline "Hit!"; 
       Legal {
         p1_enemy = st.p1_enemy;
         p1_self = new_p1_self;
         p2_enemy = Board.place_hit st.p2_enemy spot;
         p2_self = st.p2_self; 
         p1_self_sunk = if Board.is_ship_sunk new_p1_self spot then 
             (st.p1_self_sunk + 1) else st.p1_self_sunk;
         p2_self_sunk = st.p2_self_sunk;}
      ) 
    else (print_endline "Miss!"; 
          Legal {
            p1_enemy = st.p1_enemy;
            p1_self = Board.place_miss st.p1_self spot;
            p2_enemy = Board.place_miss st.p2_enemy spot;
            p2_self = st.p2_self;
            p1_self_sunk = st.p1_self_sunk;
            p2_self_sunk = st.p2_self_sunk; }
         ) 
  else 
    Illegal
