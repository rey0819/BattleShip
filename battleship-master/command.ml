
type object_phrase = string list

type command = 
  | Try of object_phrase
  | Place of object_phrase
  | Quit
  | Score
  | Rules

exception Empty

exception Malformed

(** [string_to_list_parse str] takes str and splits it my the char space
    It then deletes all extra spaces from the list *)
let string_to_list_parse str =
  List.filter (fun s -> not (s = "")) (String.split_on_char ' ' str)

(* Documentation in the mli file. *)
let parse str =
  match (string_to_list_parse str) with
  | [] -> raise (Empty)
  | h::t when (h = "quit" && (t = [])) -> Quit 
  | h::t when h = "try" && (t<>[]) -> Try t
  | h::t when h = "score" && (t=[]) -> Score
  | h::t when h = "place" && (t <> []) -> Place t
  | h::t when h = "rules" && (t = []) -> Rules
  | _ -> raise (Malformed)
