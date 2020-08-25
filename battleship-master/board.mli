
(** The abstract type of values representing a board. *)
type t

(** [empty_guess_board] returns an empty board for a player to guess 
    to guess/shoot on. *)
val empty_guess_board : t

(** [empty_ship_board] returns an empty board for a player to place their 
    ships on. *)
val empty_ship_board : t

(** [get_placed_ship_number] returns the number of ships that have been placed
    on board t. *)
val get_placed_ship_number : t -> int

(** [get_total_ship_number] returns the number of ships that are being used
    for the current game. *)
val get_total_ship_number : t -> int

(** [get_placed_ships] returns the list of blocks that currently have ships 
    placed on them. Ex: placing a ship from A1 to A3 adds A1, A2, A3 to this 
    list. *)
val get_placed_ships : t -> (char * int) list

(** [print_board] prints the player's ship board in a certain format 
    and then prints a representation of their enemy's board in a certain 
    format, as not to give away all the positions of their enemy's ship. *)
val print_board : t -> t -> unit

(** [check_placement] checks to see if the given values between the 
    (char * int)*(char * int) input into a place command are valid. Returns 
    true if this ship can be placed, returns false if this ships placement is 
    invalid (i.e. trying to place off the board, where a ship is already placed, 
    a ship that is too long or short, a ship that has already been placed, 
    etc.) *)
val check_placement : t -> int -> ((char*int)*(char*int)) -> 
  (char * int) list-> bool

(** [place_ship] is the board [t] after spot [char*int] on the old board [t] has 
    been marked as a ship. The block [char * int] is added into the ship block 
    list for that particular ship of length [int]*)
val place_ship : t -> (char*int) -> int -> t

(** [set_length_list t int] appends num [int] to the list 
    representing the lengths of the placed ships on board [t] *)
val set_length_list: t -> int -> t 

(** [place_hit] is the board [t] after a hit has been placed at spot [char*int]
    on the old board [t]*)
val place_hit : t -> (char*int) -> t

(** [place_miss] is the board [t] after a miss has been placed at spot 
    [char*int] on the old board [t]*)
val place_miss : t -> (char*int) -> t

(** [get_shot] is a bool that is true if the spot [char*int] is on the board and 
    has not been guessed before, meainging it's status is nothing and 
    false otherwise*)
val get_shot : t -> (char*int) -> bool

(** [is_ship_sunk] is true if the shipt that contains spot [char*int] has been 
    fully been sunk on board [t] and false otherwise. Fully been sunk being 
    defined as all blocks of that ship having a status of HIT*)
val is_ship_sunk : t -> (char*int) -> bool

(** [is_hit] is true if the spot [char*int] has been hit on board [t] and
    false otherwise*)
val is_hit : t -> (char*int) -> bool