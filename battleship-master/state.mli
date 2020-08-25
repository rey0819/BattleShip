(** The abstract type of values representing the game state. *)
type t 

(** [init_state t] is the initial state of the game when playing battleship. 
    The state has four boards, a player and enemy board for both players. When
     using the AI, two of the boards belong to the AI instead. All boards
     are initialized as an empty 10 by 10. This also keeps track of how many
     ships each player has sunk *)
val init_state : t

(** The type [result] represents if a move was Legal or Illegal. If the move
    was Legal, result can be decomposed into a state*)
type result = Legal of t | Illegal

(** [p1_enemy_board st] is the enemy board from state t *)
val p1_enemy_board : t -> Board.t

(** [p1_player_board st] is the player board from state t *)
val p1_player_board : t -> Board.t

(** [p2_enemy_board st] is the enemy board from state t *)
val p2_enemy_board : t -> Board.t

(** [p2_player_board st] is the player board from state t *)
val p2_player_board : t -> Board.t

(** [p1_self_sunk st] is the amount of ships sunk on P1's side *)
val p1_self_sunk : t -> int

(** [p2_self_sunk st] is the amount of ships sunk on P2's side *)
val p2_self_sunk : t -> int

(** [get_p1_score st] is the amount of ships P1 has sunk on P2's side *)
val get_p1_score: t -> int

(** [get_p2_score st] is the amount of ships P2 has sunk on P1's side *)
val get_p2_score: t -> int

(** [p1_place ((c,i),(c',i')) st] is a result representing a 
    new state with a ship placed from (c,i) to (c',i') on P1's self board*)
val p1_place : ((char*int)*(char*int)) -> t -> result

(** [p2_place ((c,i),(c',i')) st] is a result representing a 
    new state with a ship placed from (c,i) to (c',i') on P2's self board*)
val p2_place : ((char*int)*(char*int)) -> t -> result

(** [p1_guess lst st] is a result representing a 
    new state with a hit or miss placed at the element in list on P1's 
    enemy board and P2's self board*)
val p1_guess : (char*int) list -> t -> result

(** [p2_guess lst st] is a result representing a 
    new state with a hit or miss placed at the element in list on P2's 
    enemy board and P1's self board*)
val p2_guess : (char*int) list -> t -> result


