
(* -----------------------Two Player Code -----------------------------------*)

let rec ship_parse_helper lst = 
  match lst with 
  | h::t -> 
    let integer = int_of_string(String.sub h 1 (String.length h-1 )) in 
    (String.get (String.sub h 0 1) 0 , integer)::ship_parse_helper t
  | _ -> []

let ship_parse lst =
  let place_list = ship_parse_helper lst in 
  (List.hd place_list, List.hd (List.tl place_list))

let guess_parse spot =
  ship_parse_helper spot

let clear x = 
  match Sys.command("clear")+x with 
  | _ -> ()

let rec play_loop st =
  ANSITerminal.(print_string [magenta] "Player 1's Turn \n");
  let enemy_board = State.p1_enemy_board st in 
  let player_board = State.p1_player_board st in 
  Board.print_board player_board enemy_board;
  let () = print_string "\n \nType your command: " in
  let i = read_line () in
  try let com = Command.parse i in
    match com with
    | Quit -> ANSITerminal.(print_string [cyan] "Thanks for playing!\n"); 
      exit 0
    | Score -> ANSITerminal.(print_string [yellow] 
                               ("Score: " ^ 
                                string_of_int (State.get_p1_score st) ^ 
                                " ships sunk \n") ); 
      play_loop st
    | Place t -> begin match 
          let places = ship_parse t in 
          (State.p1_place places st) with 
      | Legal t -> 
        let num_of_placed_ships = Board.get_placed_ship_number player_board in
        let total_ships = Board.get_total_ship_number player_board in
        if ((num_of_placed_ships) + 1 = total_ships) then 
          (clear 1; 
           print_endline "Pass the Computer to Player 2"; 
           Unix.sleepf 3.0; 
           second_player t) 
        else play_loop t 
      | Illegal -> ANSITerminal.(print_string [red] "Invalid ship placement\n"); 
        play_loop st
      end
    | Try t -> begin 
        let input = guess_parse t in
        match (State.p1_guess input st) with
        | Legal t -> if (Board.is_ship_sunk (State.p2_player_board t) 
                           (List.hd input)) then 
            (print_endline "You sunk a ship!"; 
             if (State.p2_self_sunk t = 4) then
               (ANSITerminal.(print_string [cyan] "Player 1 has won!\n"); 
                exit 0)
             else 
               (Unix.sleepf 1.5;
                clear 1; 
                print_endline "Pass the Computer to Player 2"; 
                Unix.sleepf 3.0; 
                second_player t) 
            )else
            (Unix.sleepf 1.5;
             clear 1; 
             print_endline "Pass the Computer to Player 2"; 
             Unix.sleepf 3.0; 
             second_player t) 
        | Illegal -> 
          ANSITerminal.(print_string [red] 
                          ("Invalid Guess: Either you have guessed this spot " ^ 
                           "already or it is not on the board \n"));
          play_loop st
      end  
    | Rules -> 
      print_endline "\nRules for BattleShip: \n";
      print_endline 
        "The goal of battleship is to sink all of the opponent's ships. \n";
      print_string "You have two grids, the top is your own and the bottom is ";
      print_endline 
        (" a representation of the enemy's board." ^
         "  It contains all hits and misses.\n"); 
      print_endline "To start the game: ";
      print_endline "Place 4 ships";
      print_endline 
        "When both players have placed all of their ships the game starts.";
      print_string "When the game starts you begin guessing. ";
      print_endline "You can either hit or miss a ship when you guess.";
      print_endline 
        "The grid is 10 by 10, with rows 1 through 10 and columns A through J.";
      print_string "To designate a specific spot, you put letter first ";
      print_endline "then number second. Ex: A1";
      print_endline "\nYou have a number of moves:  ";
      print_endline "Use place spot to place a ship. Ex: place A1 A2 ";
      print_endline "Use try spot to atempt to hit a ship. Ex: try A1 ";
      print_endline "Use score to get your score";
      print_endline "Use rules to get the rules of the game\n";
      play_loop st 

  with 
  |Command.Malformed -> 
    ANSITerminal.(print_string [red] "Please type a valid command\n"); 
    play_loop st 
  | Command.Empty -> 
    ANSITerminal.(print_string [red] "Please type a command\n"); 
    play_loop st
  | Failure _ -> ANSITerminal.(print_string [red] "Invalid Command \n"); 
    play_loop st

and second_player st =
  ANSITerminal.(print_string [green] "Player 2's Turn \n");
  let enemy_board = State.p2_enemy_board st in 
  let player_board = State.p2_player_board st in 
  Board.print_board player_board enemy_board;
  let () = print_string "\n \nType your command: " in
  let i = read_line () in
  try let com = Command.parse i in
    match com with
    | Quit -> ANSITerminal.(print_string [cyan] "Thanks for playing!\n"); 
      exit 0
    | Score -> ANSITerminal.(print_string [yellow] 
                               ("Score: " ^ 
                                string_of_int (State.get_p2_score st) ^ 
                                " ships sunk \n") ); 
      second_player st
    | Place t -> begin match 
          let places = ship_parse t in 
          (State.p2_place places st) with 
      | Legal t -> 
        let num_places = Board.get_placed_ship_number player_board in 
        let total_num = Board.get_total_ship_number player_board in 
        if ((num_places) + 1 = total_num) then 
          (clear 1; 
           print_endline "Pass the Computer to Player 1"; 
           Unix.sleepf 3.0; 
           play_loop t)  
        else second_player t
      | Illegal -> ANSITerminal.(print_string [red] "Invalid ship placement\n"); 
        second_player st
      end
    | Try t -> begin 
        let input = guess_parse t in
        match (State.p2_guess input st) with
        | Legal t -> if (Board.is_ship_sunk (State.p1_player_board t) 
                           (List.hd input)) then 
            (print_endline "You sunk a ship!"; 
             if (State.p1_self_sunk t = 4) then
               (ANSITerminal.(print_string [cyan] "Player 2 has won!\n"); 
                exit 0)
             else (Unix.sleepf 1.5;
                   clear 1; 
                   print_endline "Pass the Computer to Player 1"; 
                   Unix.sleepf 3.0; 
                   play_loop t) ) 
          else
            (Unix.sleepf 1.5;
             clear 1; 
             print_endline "Pass the Computer to Player 1"; 
             Unix.sleepf 3.0; 
             play_loop t) 
        | Illegal -> 
          ANSITerminal.(print_string [red] 
                          ("Invalid Guess: Either you have guessed this spot " ^ 
                           "already or it is not on the board \n"));
          second_player st
      end
    | Rules -> 
      print_endline "\nRules for BattleShip: \n";
      print_endline 
        "The goal of battleship is to sink all of the opponent's ships. \n";
      print_string "You have two grids, the top is your own and the bottom is ";
      print_endline 
        (" a representation of the enemy's board." ^
         "  It contains all hits and misses.\n"); 
      print_endline "To start the game: ";
      print_endline "Place 4 ships";
      print_endline 
        "When both players have placed all of their ships the game starts.";
      print_string "When the game starts you begin guessing. ";
      print_endline "You can either hit or miss a ship when you guess.";
      print_endline 
        "The grid is 10 by 10, with rows 1 through 10 and columns A through J.";
      print_string "To designate a specific spot, you put letter first ";
      print_endline "then number second. Ex: A1";
      print_endline "\nYou have a number of moves:  ";
      print_endline "Use place spot to place a ship. Ex: place A1 A2 ";
      print_endline "Use try spot to atempt to hit a ship. Ex: try A1 ";
      print_endline "Use score to get your score";
      print_endline "Use rules to get the rules of the game\n";
      second_player st 
  with
  |Command.Malformed -> 
    ANSITerminal.(print_string [red] "Please type a valid command\n"); 
    second_player st 
  | Command.Empty -> 
    ANSITerminal.(print_string [red] "Please type a command\n"); 
    second_player st
  | Failure _ -> ANSITerminal.(print_string [red] "Invalid command \n"); 
    second_player st



(* -----------------------Easy AI Code -----------------------------------*)

type move = P | T 

let all_blocks =
  [ 
    "A1" ; "A2"; "A3"; "A4"; "A5";
    "A6" ; "A7" ; "A8" ; "A9" ; "A10" ;
    "B1" ; "B2" ; "B3" ; "B4" ; "B5" ;
    "B6" ; "B7" ; "B8" ; "B9" ; "B10" ;
    "C1" ; "C2" ; "C3" ; "C4" ; "C5" ;
    "C6" ; "C7" ; "C8" ; "C9" ; "C10";
    "D1" ; "D2" ; "D3" ; "D4" ; "D5";
    "D6" ; "D7" ; "D8" ; "D9" ; "D10";
    "E1" ; "E2" ; "E3" ; "E4" ; "E5" ;
    "E6" ; "E7" ; "E8" ; "E9" ; "E10";
    "F1" ; "F2" ; "F3" ; "F4" ; "F5" ;
    "F6" ; "F7" ; "F8" ; "F9" ; "F10" ;
    "G1" ; "G2" ; "G3" ; "G4" ; "G5" ;
    "G6" ; "G7" ; "G8" ; "G9" ; "G10" ;
    "H1" ; "H2" ; "H3" ; "H4" ; "H5" ;
    "H6" ; "H7" ; "H8" ; "H9" ; "H10" ;
    "I1" ; "I2" ; "I3" ; "I4" ; "I5" ;
    "I6" ; "I7" ; "I8" ; "I9" ; "I10" ;
    "J1" ; "J2" ; "J3" ; "J4" ; "J5" ;
    "J6" ; "J7" ; "J8" ; "J9" ; "J10" ;
  ]


let rec play_AI st ai_block_lst : unit=
  ANSITerminal.(print_string [magenta] "Your turn! \n");
  let enemy_board = State.p1_enemy_board st in 
  let player_board = State.p1_player_board st in 
  Board.print_board player_board enemy_board;
  let () = print_string "\n \nType your command: " in
  let i = read_line () in
  try let com = Command.parse i in
    match com with
    | Quit -> ANSITerminal.(print_string [cyan] "Thanks for playing!\n"); 
      exit 0
    | Score -> ANSITerminal.(print_string [yellow] 
                               ("Score: " ^ 
                                string_of_int (State.get_p1_score st) ^ 
                                " ships sunk \n") ); 
      play_AI st ai_block_lst
    | Place t -> begin match 
          let places = ship_parse t in 
          (State.p1_place places st) with 
      | Legal t -> 
        let num_placed = Board.get_placed_ship_number player_board in 
        let total_num = Board.get_total_ship_number player_board in 
        if ((num_placed) + 1 = total_num) then 
          (
            print_endline "It is the AI's turn"; 
            Unix.sleepf 2.0; 
            ai_move t P ai_block_lst) 
        else play_AI t ai_block_lst
      | Illegal -> ANSITerminal.(print_string [red] "Invalid ship placement\n"); 
        play_AI st ai_block_lst
      end
    | Try t -> begin 
        let input = guess_parse t in
        match (State.p1_guess input st) with
        | Legal t -> if (Board.is_ship_sunk (State.p2_player_board t) 
                           (List.hd input)) then 
            (print_endline "You sunk a ship!"; 
             if (State.p2_self_sunk t = 4) then
               (ANSITerminal.(print_string [cyan] "You have won!\n"); exit 0)
             else 
               (Unix.sleepf 1.5;
                (* clear 1;  *)
                print_endline "It is the AI's turn"; 
                Unix.sleepf 2.0; 
                ai_move t T ai_block_lst) 
            )else
            (Unix.sleepf 1.5;
             (* clear 1;  *)
             print_endline "It is the AI's turn"; 
             Unix.sleepf 2.0; 
             ai_move t T ai_block_lst) 
        | Illegal -> 
          ANSITerminal.(print_string [red] 
                          ("Invalid Guess: Either you have guessed this spot " ^ 
                           "already or it is not on the board \n"));
          play_AI st ai_block_lst
      end   
    | Rules -> 
      print_endline "\nRules for BattleShip: \n";
      print_endline 
        "The goal of battleship is to sink all of the opponent's ships. \n";
      print_string "You have two grids, the top is your own and the bottom is ";
      print_endline 
        (" a representation of the enemy's board." ^
         "  It contains all hits and misses.\n"); 
      print_endline "To start the game: ";
      print_endline "4 ships";
      print_endline 
        "When both players have placed all of their ships the game starts.";
      print_string "When the game starts you begin guessing. ";
      print_endline "You can either hit or miss a ship when you guess.";
      print_endline 
        "The grid is 10 by 10, with rows 1 through 10 and columns A through J.";
      print_string "To designate a specific spot, you put letter first ";
      print_endline "then number second. Ex: A1";
      print_endline "\nYou have a number of moves:  ";
      print_endline "Use place spot to place a ship. Ex: place A1 A2 ";
      print_endline "Use try spot to atempt to hit a ship. Ex: try A1 ";
      print_endline "Use score to get your score";
      print_endline "Use rules to get the rules of the game\n";
      play_AI st ai_block_lst
  with 
  |Command.Malformed -> 
    ANSITerminal.(print_string [red] "Please type a valid command\n"); 
    play_AI st ai_block_lst
  | Command.Empty -> 
    ANSITerminal.(print_string [red] "Please type a command\n"); 
    play_AI st ai_block_lst
  | Failure _ -> ANSITerminal.(print_string [red] "Invalid Command \n"); 
    play_AI st ai_block_lst

and ai_move st move ai_block_lst =
  match move with 
  | P -> 
    let start_block = List.nth all_blocks (Random.int 100) in
    let result = place_block start_block sizes in
    let place_string = fst result in 
    let input = ship_parse place_string in 
    ai_place input sizes st ai_block_lst
  | T -> 
    let result = random_block ai_block_lst in 
    let random_guess = [fst result] in 
    let new_list = snd result in 
    let input = guess_parse random_guess in 
    match (State.p2_guess input st) with
    | Legal t -> if (Board.is_ship_sunk (State.p1_player_board t) 
                       (List.hd input)) then 
        (print_endline "The AI sunk a ship :("; 
         if (State.p1_self_sunk t = 4) then
           (ANSITerminal.(print_string [cyan] "The AI has won!\n"); exit 0)
         else 
           (
             print_endline "It is your turn"; 
             play_AI t new_list) 
        )else
        (play_AI t new_list) 
    | Illegal ->
      play_AI st new_list


and ai_place places size_list st ai_block_list= 
  match size_list with 
  | [] -> play_AI st ai_block_list
  | h::t ->             
    begin match (State.p2_place places st) with 
      | Legal f -> 
        let start_block = List.nth all_blocks 
            (Random.int (List.length all_blocks)) in
        let result = place_block start_block size_list in
        let lst = snd result in 
        let place_string = fst result in 
        let input = ship_parse place_string in 
        ai_place input lst f ai_block_list
      | Illegal ->
        let start_block = List.nth all_blocks 
            (Random.int (List.length all_blocks)) in
        let result = place_block start_block size_list in
        let place_string = fst result in 
        let input = ship_parse place_string in 
        ai_place input size_list st ai_block_list
    end

and random_block blocks =
  let b = List.nth blocks (Random.int (List.length blocks)) in
  (b , (List.filter (fun bl -> bl <> b) blocks))

and sizes =
  [
    2; 3; 4; 5;
  ]

and place_block bl size_list =
  let s = List.nth size_list (Random.int (List.length size_list)) in
  let rand = Random.int 2 in 
  if(rand = 0) (*horizontal*)
  then 
    let l = String.sub bl 0 1 in 
    let n = int_of_string(String.sub bl 1 ((String.length bl) - 1)) in
    if(n + s - 1< 11) 
    then ( [bl ; (l ^ string_of_int(n + s - 1))] , 
           (List.filter (fun sz -> sz <> s) size_list))
    else ( [(l ^ string_of_int(n - s + 1)) ; bl], 
           (List.filter (fun sz -> sz <> s) size_list))
  else (*vertical*)
    let l = Char.code(String.get (String.sub bl 0 1) 0)  in 
    let n = String.sub bl 1 ((String.length bl) - 1) in
    if(l + s - 1 < Char.code('K')) 
    then ([bl; ((Char.escaped (Char.chr(l + s - 1))) ^ n)] , 
          List.filter (fun sz -> sz <> s) size_list)
    else ( [((Char.escaped (Char.chr(l - s + 1))) ^ n) ; bl] , 
           List.filter (fun sz -> sz <> s) size_list)



(* -----------------------Medium AI Code -----------------------------------*)

type medium_move = P2 | T2 

let all_blocks =
  [ 
    "A1" ; "A2"; "A3"; "A4"; "A5";
    "A6" ; "A7" ; "A8" ; "A9" ; "A10" ;
    "B1" ; "B2" ; "B3" ; "B4" ; "B5" ;
    "B6" ; "B7" ; "B8" ; "B9" ; "B10" ;
    "C1" ; "C2" ; "C3" ; "C4" ; "C5" ;
    "C6" ; "C7" ; "C8" ; "C9" ; "C10";
    "D1" ; "D2" ; "D3" ; "D4" ; "D5";
    "D6" ; "D7" ; "D8" ; "D9" ; "D10";
    "E1" ; "E2" ; "E3" ; "E4" ; "E5" ;
    "E6" ; "E7" ; "E8" ; "E9" ; "E10";
    "F1" ; "F2" ; "F3" ; "F4" ; "F5" ;
    "F6" ; "F7" ; "F8" ; "F9" ; "F10" ;
    "G1" ; "G2" ; "G3" ; "G4" ; "G5" ;
    "G6" ; "G7" ; "G8" ; "G9" ; "G10" ;
    "H1" ; "H2" ; "H3" ; "H4" ; "H5" ;
    "H6" ; "H7" ; "H8" ; "H9" ; "H10" ;
    "I1" ; "I2" ; "I3" ; "I4" ; "I5" ;
    "I6" ; "I7" ; "I8" ; "I9" ; "I10" ;
    "J1" ; "J2" ; "J3" ; "J4" ; "J5" ;
    "J6" ; "J7" ; "J8" ; "J9" ; "J10" ;
  ]


let rec play_medium_AI st ai_block_lst last_block: unit=
  ANSITerminal.(print_string [magenta] "Your turn! \n");
  let enemy_board = State.p1_enemy_board st in 
  let player_board = State.p1_player_board st in 
  Board.print_board player_board enemy_board;
  let () = print_string "\n \nType your command: " in
  let i = read_line () in
  try let com = Command.parse i in
    match com with
    | Quit -> ANSITerminal.(print_string [cyan] "Thanks for playing!\n"); 
      exit 0
    | Score -> ANSITerminal.(print_string [yellow] 
                               ("Score: " ^ 
                                string_of_int (State.get_p1_score st) ^ 
                                " ships sunk \n") ); 
      play_medium_AI st ai_block_lst last_block
    | Place t -> begin match 
          let places = ship_parse t in 
          (State.p1_place places st) with 
      | Legal t -> 
        let num_placed = Board.get_placed_ship_number player_board in 
        let total_num = Board.get_total_ship_number player_board in 
        if ((num_placed) + 1 = total_num) then 
          (
            print_endline "It is the AI's turn"; 
            Unix.sleepf 2.0; 
            ai_medium_move t P2 ai_block_lst last_block) 
        else play_medium_AI t ai_block_lst last_block
      | Illegal -> ANSITerminal.(print_string [red] "Invalid ship placement\n"); 
        play_medium_AI st ai_block_lst last_block
      end
    | Try t -> begin 
        let input = guess_parse t in
        match (State.p1_guess input st) with
        | Legal t -> if (Board.is_ship_sunk (State.p2_player_board t) 
                           (List.hd input)) then 
            (print_endline "You sunk a ship!"; 
             if (State.p2_self_sunk t = 4) then
               (ANSITerminal.(print_string [cyan] "You have won!\n"); exit 0)
             else 
               (Unix.sleepf 1.5;
                (* clear 1;  *)
                print_endline "It is the AI's turn"; 
                Unix.sleepf 2.0; 
                ai_medium_move t T2 ai_block_lst last_block) 
            )else
            (Unix.sleepf 1.5;
             (* clear 1;  *)
             print_endline "It is the AI's turn"; 
             Unix.sleepf 2.0; 
             ai_medium_move t T2 ai_block_lst last_block) 
        | Illegal -> 
          ANSITerminal.(print_string [red] 
                          ("Invalid Guess: Either you have guessed this spot " ^ 
                           "already or it is not on the board \n"));
          play_medium_AI st ai_block_lst last_block
      end   
    | Rules -> 
      print_endline "\nRules for BattleShip: \n";
      print_endline 
        "The goal of battleship is to sink all of the opponent's ships. \n";
      print_string "You have two grids, the top is your own and the bottom is ";
      print_endline 
        (" a representation of the enemy's board." ^
         "  It contains all hits and misses.\n"); 
      print_endline "To start the game: ";
      print_endline "Place 5 ships";
      print_endline 
        "When both players have placed all of their ships the game starts.";
      print_string "When the game starts you begin guessing. ";
      print_endline "You can either hit or miss a ship when you guess.";
      print_endline 
        "The grid is 10 by 10, with rows 1 through 10 and columns A through J.";
      print_string "To designate a specific spot, you put letter first ";
      print_endline "then number second. Ex: A1";
      print_endline "\nYou have a number of moves:  ";
      print_endline "Use place spot to place a ship. Ex: place A1 A2 ";
      print_endline "Use try spot to atempt to hit a ship. Ex: try A1 ";
      print_endline "Use score to get your score";
      print_endline "Use rules to get the rules of the game\n";
      play_medium_AI st ai_block_lst last_block
  with 
  |Command.Malformed -> 
    ANSITerminal.(print_string [red] "Please type a valid command\n"); 
    play_medium_AI st ai_block_lst last_block
  | Command.Empty -> 
    ANSITerminal.(print_string [red] "Please type a command\n"); 
    play_medium_AI st ai_block_lst last_block
  | Failure _ -> ANSITerminal.(print_string [red] "Invalid Command \n"); 
    play_medium_AI st ai_block_lst last_block

and ai_medium_move st move (ai_block_lst: string list) last_block=
  let enemy_board = State.p2_enemy_board st in 
  match move with 
  | P2 -> 
    let start_block = List.nth all_blocks (Random.int 100) in
    let result = place_medium_block start_block sizes in
    let place_string = fst result in 
    let input = ship_parse place_string in 
    ai_medium_place input sizes st ai_block_lst last_block
  | T2 -> 
    if Board.is_hit enemy_board last_block then 
      smart_guess last_block ai_block_lst st
    else
      dumb_guess ai_block_lst st


and dumb_guess ai_block_lst st = 
  let result = random_block ai_block_lst in 
  let random_guess = [fst result] in 
  let new_list = snd result in 
  let input = guess_parse random_guess in 
  let current_block = List.hd input in
  match (State.p2_guess input st) with
  | Legal t -> if (Board.is_ship_sunk (State.p1_player_board t) 
                     (List.hd input)) then 
      (print_endline "The AI sunk a ship :("; 
       if (State.p1_self_sunk t = 4) then
         (ANSITerminal.(print_string [cyan] "The AI has won!\n"); exit 0)
       else 
         (
           print_endline "It is your turn"; 
           play_medium_AI t new_list current_block) 
      )else
      (play_medium_AI t new_list current_block) 
  | Illegal ->
    play_medium_AI st new_list current_block

and smart_guess last_block block_list state=
  let list_of_possible = (smart_guess_helper last_block block_list) in
  if (List.length list_of_possible <> 0) then
    let close_guess = 
      List.nth list_of_possible (Random.int (List.length list_of_possible)) in 
    let string_guess = convert_to_string close_guess in 
    let new_list = List.filter (fun bl -> bl <> string_guess) block_list in 
    match (State.p2_guess [close_guess] state) with
    | Legal t -> if (Board.is_ship_sunk (State.p1_player_board t) 
                       close_guess) then 
        (print_endline "The AI sunk a ship :("; 
         if (State.p1_self_sunk t = 4) then
           (ANSITerminal.(print_string [cyan] "The AI has won!\n"); exit 0)
         else 
           (
             print_endline "It is your turn"; 
             play_medium_AI t new_list close_guess) 
        )else
        (play_medium_AI t new_list close_guess) 
    | Illegal ->
      play_medium_AI state new_list close_guess
  else dumb_guess block_list state


and smart_guess_helper last_block block_list = 
  let list_of_possible = [] in 
  let list1 = 
    if (snd last_block > 1) 
    then (fst last_block,snd last_block - 1)::list_of_possible
    else list_of_possible in 
  let list2 = 
    if (snd last_block < 10) 
    then (fst last_block , snd last_block + 1)::list1
    else list1 in 
  let list3 = 
    let ascii = Char.code (fst last_block) in 
    if (ascii > 65) 
    then (Char.chr (ascii - 1), snd last_block)::list2 
    else list2 in 
  let list4 = 
    let ascii = Char.code (fst last_block) in 
    if (ascii < 74) 
    then (Char.chr (ascii + 1), snd last_block)::list3 
    else list3 in 
  List.filter (fun s -> List.mem (convert_to_string s) block_list) list4

and convert_to_string spot = 
  let chr = Char.escaped (fst spot)in 
  let num = string_of_int (snd spot) in 
  chr ^ num


and ai_medium_place places size_list st ai_block_list current_block= 
  match size_list with 
  | [] -> play_medium_AI st ai_block_list current_block
  | h::t ->             
    begin match (State.p2_place places st) with 
      | Legal f -> 
        let start_block = List.nth all_blocks 
            (Random.int (List.length all_blocks)) in
        let result = place_medium_block start_block size_list in
        let lst = snd result in 
        let place_string = fst result in 
        let input = ship_parse place_string in 
        ai_medium_place input lst f ai_block_list current_block
      | Illegal ->
        let start_block = List.nth all_blocks 
            (Random.int (List.length all_blocks)) in
        let result = place_medium_block start_block size_list in
        let place_string = fst result in 
        let input = ship_parse place_string in 
        ai_medium_place input size_list st ai_block_list current_block
    end

and random_block blocks =
  let b = List.nth blocks (Random.int (List.length blocks)) in
  (b , (List.filter (fun bl -> bl <> b) blocks))

and sizes =
  [
    2; 3; 4; 5;
  ]

and place_medium_block bl size_list =
  let s = List.nth size_list (Random.int (List.length size_list)) in
  let rand = Random.int 2 in 
  if(rand = 0) (*horizontal*)
  then 
    let l = String.sub bl 0 1 in 
    let n = int_of_string(String.sub bl 1 ((String.length bl) - 1)) in
    if(n + s - 1< 11) 
    then ( [bl ; (l ^ string_of_int(n + s - 1))] , 
           (List.filter (fun sz -> sz <> s) size_list))
    else ( [(l ^ string_of_int(n - s + 1)) ; bl], 
           (List.filter (fun sz -> sz <> s) size_list))
  else (*vertical*)
    let l = Char.code(String.get (String.sub bl 0 1) 0)  in 
    let n = String.sub bl 1 ((String.length bl) - 1) in
    if(l + s - 1 < Char.code('K')) 
    then ([bl; ((Char.escaped (Char.chr(l + s - 1))) ^ n)] , 
          List.filter (fun sz -> sz <> s) size_list)
    else ( [((Char.escaped (Char.chr(l - s + 1))) ^ n) ; bl] , 
           List.filter (fun sz -> sz <> s) size_list)





(* -----------------------Main Code -----------------------------------*)


(** [main ()] prompts for the game to play, then starts it. *)
let rec main () : unit =
  ANSITerminal.(print_string [yellow] 
                  ("Welcome to Battleship! Would you like to" ^ 
                   "play a two-player game or against an AI?\n"));
  ANSITerminal.(print_string [yellow] "Input 1 for a two-player game.\n");
  ANSITerminal.(print_string [yellow] "Input 2 for an AI:\n");
  let i = read_line () in
  let state = State.init_state in
  match i with 
  | "1" ->
    ANSITerminal.(print_string [yellow] "You have chosen a two player game! \n");
    ANSITerminal.(print_string [yellow] 
                    ("Please place your ships using the" ^ 
                     "'place' command (ex: 'place A1 A3') or use the" ^  
                     "'rules' command to learn the rules of the game\n \n"));
    play_loop state
  | "2" -> choose_ai state
  | _ -> ANSITerminal.(print_string [red] 
                         ("Invalid input, You have defaulted " ^ 
                          "to a two player game.\n")); 
    ANSITerminal.(print_string [yellow]           
                    ("Please place your ships using the" ^ 
                     "'place' command (ex: 'place A1 A3') or use the" ^  
                     "'rules' command to learn the rules of the game\n \n"));
    play_loop state


and choose_ai state = 
  ANSITerminal.(print_string [yellow] "You have chosen an AI! \n");
  ANSITerminal.(print_string [yellow] "Input easy for an easy AI\n");
  ANSITerminal.(print_string [yellow] "Input medium for a harder AI\n");
  let i2 = read_line () in
  match i2 with 
  | "easy" ->
    ANSITerminal.(print_string [yellow] "You have chosen an easy AI! \n");
    ANSITerminal.(print_string [yellow] 
                    ("Please place your ships using the" ^ 
                     "'place' command (ex: 'place A1 A3') or use the" ^  
                     "'rules' command to learn the rules of the game\n \n"));
    Random.self_init ();
    play_AI state all_blocks
  | "medium" ->
    ANSITerminal.(print_string [yellow] "You have chosen a medium AI! \n");
    ANSITerminal.(print_string [yellow] 
                    ("Please place your ships using the" ^ 
                     "'place' command (ex: 'place A1 A3') or use the" ^  
                     "'rules' command to learn the rules of the game\n \n"));
    Random.self_init ();
    play_medium_AI state all_blocks ('P', 1)
  | _-> 
    ANSITerminal.(print_string [red] 
                    ("Invalid input, You have defaulted " ^ 
                     "to an easy AI.\n")); 
    ANSITerminal.(print_string [yellow]           
                    ("Please place your ships using the" ^ 
                     "'place' command (ex: 'place A1 A3') or use the" ^  
                     "'rules' command to learn the rules of the game\n \n"));
    play_AI state all_blocks



let () = main ()
