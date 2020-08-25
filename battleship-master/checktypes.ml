module type BoardSig = sig
  type t
  val empty_guess_board : t
  val empty_ship_board : t
  val print_board : t -> t -> unit
  val check_placement : t -> int -> ((char*int)*(char*int)) -> (char * int) list-> bool
  val place_ship : t -> (char*int) -> int -> t
  val set_length_list: t -> int -> t 
end

module BoardCheck : BoardSig = Board

module type CommandSig = sig
  type object_phrase = string list
  type command = 
    |Try of object_phrase  
    | Place of object_phrase
    | Quit
    | Score
    | Rules
  exception Empty
  exception Malformed
  val parse : string -> command
end

module CommandCheck : CommandSig = Command

module type StateSig = sig
  type t 
  val init_state : t
  val enemy_board : t -> Board.t
  type result = Legal of t | Illegal
  val place : ((char*int)*(char*int)) -> t -> result
end

module StateCheck : StateSig = State

module type AuthorSig = sig
  val hours_worked : int
end

module AuthorCheck : AuthorSig = Author
