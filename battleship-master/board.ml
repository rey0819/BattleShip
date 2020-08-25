type shot = Hit|Miss|Nothing

type bt = Guess|Ship

type block = {position:(char * int); 
              has_ship: bool;
              status: shot;
              format: string}

type ship = {name:string; 
             size:int;
             blocks: (char*int) list;
             sunk : bool}

type t = {board_type:bt;
          grid: (block list) list; 
          ships: ship list; 
          placed_ships: (char * int) list;
          placed_ship_lengths: int list;}

let rec init_row_helper row n = 
  if n > 0 then {position = (row, 11 - n); 
                 has_ship = false; 
                 status = Nothing; 
                 format = "-"}::(init_row_helper row (n-1))
  else []

let rec init_col_helper r c = 
  if r > 0 then
    match r with 
    | 10 -> (init_row_helper 'A' c)::(init_col_helper (r-1) c)
    | 9 -> (init_row_helper 'B' c)::(init_col_helper (r-1) c)
    | 8 -> (init_row_helper 'C' c)::(init_col_helper (r-1) c)
    | 7 -> (init_row_helper 'D' c)::(init_col_helper (r-1) c)
    | 6 -> (init_row_helper 'E' c)::(init_col_helper (r-1) c)
    | 5 -> (init_row_helper 'F' c)::(init_col_helper (r-1) c)
    | 4 -> (init_row_helper 'G' c)::(init_col_helper (r-1) c)
    | 3 -> (init_row_helper 'H' c)::(init_col_helper (r-1) c)
    | 2 -> (init_row_helper 'I' c)::(init_col_helper (r-1) c)
    | 1 -> (init_row_helper 'J' c)::(init_col_helper (r-1) c)
    | _ -> (init_row_helper 'Z' c)::(init_col_helper (r-1) c)
  else []

(** [empty_ship_board] is the intial ship board *)
let empty_ship_board = 
  {
    board_type = Ship;
    grid = init_col_helper 10 10;
    ships = [{name = "Carrier"; size = 5; blocks = []; sunk = false};
             {name = "Battleship"; size = 4; blocks = []; sunk = false};
             {name = "Cruiser"; size = 3; blocks = []; sunk = false};
             {name = "Destroyer"; size = 2; blocks = []; sunk = false}];
    placed_ships =[];
    placed_ship_lengths = [];
  }

(** [empty_guess_board] is the intial guess board *)
let empty_guess_board = 
  {
    board_type = Guess;
    grid = init_col_helper 10 10;
    ships = [{name = "Carrier"; size = 5; blocks = []; sunk = false};
             {name = "Battleship"; size = 4; blocks = []; sunk = false};
             {name = "Cruiser"; size = 3; blocks = []; sunk = false};
             {name = "Destroyer"; size = 2; blocks = []; sunk = false}];
    placed_ships =[];
    placed_ship_lengths = [];
  }

let get_placed_ship_number b =
  List.length b.placed_ship_lengths 

let get_total_ship_number b =
  List.length b.ships

let get_placed_ships b =
  b.placed_ships

let  check_placement_helper brd range =
  let r = [fst range; snd range] in 
  let rec func b rng = 
    match rng with
    | h::t -> if List.exists (fun x -> h = x) b.placed_ships then false 
      else func b t
    | _ -> true
  in func brd r


(* All chars must be uppercased *)
let check_placement brd size (range: (char*int)*(char*int)) range_list : bool =
  let check_size = List.exists (fun x -> x.size = size) brd.ships in 
  let check_placed_ships = List.exists (fun x -> x = size) 
      brd.placed_ship_lengths in 
  let check_range = List.exists (fun x -> List.exists (fun y -> y = x) 
                                    brd.placed_ships) range_list in 
  let actual_size = size -1 in 
  let fst_char = Char.code(fst (fst range)) in 
  let snd_char = Char.code (fst (snd range)) in
  let fst_int = snd (fst range) in
  let snd_int = snd (snd range) in
  (* false statements *)
  let a = (fst_char < 64) || (fst_char > 75) in 
  let b = (snd_char < 64) || (snd_char > 75) in
  let e = (fst_int < 0 || fst_int > 10) in
  let f = (snd_int < 0 || snd_int > 10) in 

  (* true statements*)
  let c = Int.abs(fst_char - snd_char) = actual_size && fst_int = snd_int in 
  let d = Int.abs(fst_int-snd_int) = actual_size && fst_char = snd_char in

  if(a || b || e || f || check_placed_ships || check_range) then false
  else if c && (check_placement_helper brd range) && check_size then true   
  else if d && check_size then true
  else false

let rec get_shot_helper lst spot acc =
  match lst with 
  | h::t -> let new_acc = if ((h.position = spot) && (h.status <> Nothing)) 
              then true else false in 
    get_shot_helper t spot (new_acc || acc)
  | [] -> acc

(** Tests to see if the spot is on the board and has not been guessed before*)
let get_shot brd spot =
  let grid = brd.grid in 
  let rec g_s gr spot (acc:bool) =
    match gr with 
    | h::t -> g_s t spot (get_shot_helper h spot acc)
    | [] -> acc in
  g_s grid spot false

let rec insert_ship_block_helper lst size spot acc=
  match lst with 
  | h::t -> if h.size = size then 
      insert_ship_block_helper t size spot ({
          name= h.name; 
          size= h.size;
          blocks= spot::h.blocks;
          sunk = h.sunk}::acc)
    else 
      insert_ship_block_helper t size spot (h::acc) 
  | [] -> acc

(* Adds spot onto correct ship blocks list *)
let insert_ship_block brd size spot=
  insert_ship_block_helper brd.ships size spot []

(** Function to see if an entire ship has been sunk*)
let rec is_ship_sunk brd spot =
  let ship = get_ship brd.ships spot in 
  match ship with 
  | Some v -> all_blocks_hits v.blocks true brd spot
  | None -> false

and all_blocks_hits lst acc brd spot=
  match lst with
  | h::t -> let new_acc = if (get_status brd h) then true else false in 
    all_blocks_hits t (new_acc && acc) brd spot
  | [] -> acc

and get_ship lst spot = 
  match lst with 
  | h::t ->  if List.exists (fun x -> x = spot) h.blocks then Some h 
    else get_ship t spot 
  | [] ->  None

and get_status_helper lst spot acc =
  match lst with 
  | h::t -> let new_acc = (h.position = spot) && (h.status = Hit) in 
    get_status_helper t spot (new_acc || acc)
  | [] -> acc

and g_s grid spot acc =
  match grid with 
  | h::t -> g_s t spot ((get_status_helper h spot acc)||acc)
  | [] -> acc

(** Tests to see if the spot is on the board and has not been guessed before*)
and get_status brd spot =
  let grid = brd.grid in 
  g_s grid spot false


(** Function to see if [spot] is hit on [board]*)
let is_hit board spot =
  let grid = board.grid in 
  g_s grid spot false

and get_status_helper lst spot acc =
  match lst with 
  | h::t -> let new_acc = (h.position = spot) && (h.status = Hit) in 
    get_status_helper t spot (new_acc || acc)
  | [] -> acc

and g_s grid spot acc =
  match grid with 
  | h::t -> g_s t spot ((get_status_helper h spot acc)||acc)
  | [] -> acc

(** [place_ship] places a ship on board [brd] in position [spot] *)
let place_ship brd spot size =
  {
    board_type = Ship;
    grid =  List.map 
        (fun y -> List.map 
            (fun x -> if (x.position = spot) 
              then
                {position = spot; 
                 has_ship = true; 
                 status = Nothing; 
                 format = "S"}
              else x) y) brd.grid;
    ships = insert_ship_block brd size spot;
    placed_ships = spot::brd.placed_ships;
    placed_ship_lengths = brd.placed_ship_lengths;
  }

(** [place_hit] places a hit on board [brd] in position [spot] *)
let place_hit brd spot =
  {
    board_type = Guess;
    grid =  List.map 
        (fun y -> List.map 
            (fun x -> if (x.position = spot) 
              then
                {position = spot; 
                 has_ship = true; 
                 status = Hit; 
                 format = "H"}
              else x) y) brd.grid;
    ships = brd.ships;
    placed_ships = brd.placed_ships;
    placed_ship_lengths = brd.placed_ship_lengths;
  }

(** [place_miss] places a miss on board [brd] in position [spot] *)
let place_miss brd spot =
  {
    board_type = Guess;
    grid =  List.map 
        (fun y -> List.map 
            (fun x -> if (x.position = spot) 
              then
                {position = spot; 
                 has_ship = false; 
                 status = Miss; 
                 format = "X"}
              else x) y) brd.grid;
    ships = brd.ships;
    placed_ships = brd.placed_ships;
    placed_ship_lengths = brd.placed_ship_lengths;
  }

(** [set_length_list brd num] appends num to the list 
    representing the lengths of the placed ships *)
let set_length_list brd num =
  {board_type = brd.board_type;
   grid = brd.grid;
   ships = brd.ships;
   placed_ships = brd.placed_ships;
   placed_ship_lengths = num :: brd.placed_ship_lengths} 

let rec get_remaining_ships brd ship_list acc =
  let pl = brd.placed_ship_lengths in 
  match ship_list with
  | [] -> if acc = "" then "You have placed all the ships" else acc
  | h::t -> if(List.exists (fun s -> s = h.size) pl) then
      get_remaining_ships brd t acc else
      get_remaining_ships brd t 
        (h.name ^ "->length:" ^ string_of_int h.size ^ ", " ^ acc)


(** [print_board] prints ship board [yours] in a certain format and then
    prints guess board [theirs] in a certain format *)
let print_board (yours:t) (theirs:t) : unit = 
  ANSITerminal.(print_string  [blue] 
                  ("Ships:" ^(get_remaining_ships yours yours.ships "")^ "\n"));
  print_endline "This is your current board: \n";
  print_string "   1 2 3 4 5 6 7 8 9 10";
  let col_fun = (fun y -> print_string " "; 
                  if ((snd y.position) = 1) 
                  then (print_char (fst y.position); print_string " "); 
                  print_string y.format) in 
  let row_fun = (fun x -> print_endline ""; (List.iter col_fun x)) in
  List.iter row_fun yours.grid;
  print_endline 
    "\n \n This is a representation of your enemy's current board: \n";
  print_string "   1 2 3 4 5 6 7 8 9 10";
  let col_fun = (fun y -> print_string " "; 
                  if ((snd y.position) = 1) 
                  then (print_char (fst y.position); print_string " "); 
                  print_string y.format) in 
  let row_fun = (fun x -> print_endline ""; (List.iter col_fun x)) in
  List.iter row_fun theirs.grid;
  print_endline "";
