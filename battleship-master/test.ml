open OUnit2
open Board
open Command
open State

(* TEST PLAN
   We thought it would be best to try to test the implementation of many of our
   functions as follows below. We tested as many of our functions as we could
   by using OUnit test cases, some of these functions like [place_test] and 
   [guess_test] we tested by essentially playing a game before by creating
   different states and boards along the way. Then we could test these functions
   by trying to use them to guess and place ships then comparing the resulting
   state and board to the state and board that we got above the tests, by 
   essentially playing some of a game. We used mostly glass-box testing in our
   test suite. We also tested our parse function using OUnit to make sure it
   was giving us the correct commands and exceptions. Apart from this, we tested
   the rest of the system and game by playing games over and over and trying 
   different commands to make sure that they work. Our approach to this manual
   testing was to basically try to make our system fail, crash, or encounter a
   bug and we would fix any bugs we ran into. We believe that this was the best
   way to test our system because if our code passes all the test cases and we 
   could not get it to error while specifically trying to cause bugs, then we
   feel that the game should work near perfectly and that people just playing
   normally would not encounter any bugs. Any functions we could not test 
   automatically by using OUnit, we tested manually in many different ways to
   ensure they worked as intended.
*)

(* -----------------------Board Test-------------------------------- *)

(** [make_get_place_ship_number_test name input expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output]
    with [get_place_ship_number input]. *)
let make_get_place_ship_number_test 
    (name : string) 
    (input: Board.t) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_placed_ship_number input) 
        ~printer:string_of_int)

(** [make_is_sunk_ship_test] contructs an OUnit test named [name] that asserts 
    whether or not the ship in spot [spot] on board [input] is sunk. If [spot] 
    does is not filled with a ship, then this should return false, because 
    if ship is not there then it still is not sunk *)
let make_is_ship_sunk_test 
    (name: string)
    (input: Board.t)
    (spot: (char * int))
    (expected_output: bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_ship_sunk input spot)) 

(* -----------------------State Test-------------------------------- *)

(** [make_p1_enemy_board_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [p1_enemy_board input]. *)
let make_p1_enemy_board_test 
    (name : string) 
    (input: State.t) 
    (expected_output : Board.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (p1_enemy_board input))

(** [make_p1_player_board_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [p1_player_board input]. *)
let make_p1_player_board_test 
    (name : string) 
    (input: State.t) 
    (expected_output : Board.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (p1_player_board input))

(** [make_p2_enemy_board_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [p2_enemy_board input]. *)
let make_p2_enemy_board_test 
    (name : string) 
    (input: State.t) 
    (expected_output : Board.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (p2_enemy_board input))

(** [make_p2_player_board_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [p2_player_board input]. *)
let make_p2_player_board_test 
    (name : string) 
    (input: State.t) 
    (expected_output : Board.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (p2_player_board input))

(** [make_p1_place_test name input state expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [p1_place input state]. *)
let make_p1_place_test 
    (name : string) 
    (input: (char*int)*(char*int)) 
    (state: State.t)
    (expected_output : State.result) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (p1_place input state))

(* -----------------------Command Test-------------------------------- *)
(** [make_p1_guess_test] constructs an OUnit test named [name] that asserts the 
    quality of [expected_output] with [p1_guess input state] *)
let make_p1_guess_test
    (name : string) 
    (input: (char*int) list)
    (state: State.t)
    (expected_output : State.result) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (p1_guess input state))

(** [make_p2_guess_test] constructs an OUnit test named [name] that asserts the 
    quality of [expected_output] with [p2_guess input state] *)
let make_p2_guess_test
    (name : string) 
    (input: (char*int) list)
    (state: State.t)
    (expected_output : State.result) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (p2_guess input state))

(** [make_parse_test] constructs an OUnit test named [name] that asserts whether
    or not [parse input], where [input] is the string input by the user
    returns the correct command [expected_output]. This test only works for
    valid string inputs, [make_parse_test_exn] tests the exceptions for invalid 
    string inputs *)
let make_parse_test
    (name : string) 
    (input: string)
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse input))

(** [make_parse_test_exn] contructs an OUnit test named [name] that checks to 
    ensure that [parse input] returns the correct exception when invalid input
    strings are put into the function. The exception is either Empty or 
    Malformed*)
let make_parse_test_exn
    (name: string)
    (input: string)
    (expected_output : exn) : test = 
  name >:: (fun _->
      assert_raises expected_output (fun () -> (parse input)))

let make_p1_score_test
    (name : string) 
    (input: State.t)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_p1_score input))

let make_p2_score_test
    (name : string) 
    (input: State.t)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_p2_score input))

(** Intialize a game state with empty boardstey *)
let game_state = State.init_state

let guessBoard = Board.empty_guess_board
let shipBoard = Board.empty_ship_board

(** P1 places a ship from A1 to A2, boards and state is updated *)
let game_result_2 = State.p1_place (('A', 1),('A', 2)) game_state
let game_state_2 = match game_result_2 with 
  | Legal s -> s
  | _ -> game_state
let game_state_2_p1_e_board = (State.p1_enemy_board game_state_2)
let game_state_2_p1_p_board = (State.p1_player_board game_state_2)
let game_state_2_p2_e_board = (State.p2_enemy_board game_state_2)
let game_state_2_p2_p_board = (State.p2_player_board game_state_2)

(** P2 places a ship from B1 to B3, boards and state is updated *)
let game_result_3 = State.p2_place (('B', 1),('B', 3)) game_state_2
let game_state_3 = match game_result_3 with 
  | Legal s -> s
  | _ -> game_state 
let game_state_3_p1_e_board = (State.p1_enemy_board game_state_3)
let game_state_3_p1_p_board = (State.p1_player_board game_state_3)
let game_state_3_p2_e_board = (State.p2_enemy_board game_state_3)
let game_state_3_p2_p_board = (State.p2_player_board game_state_3)

(** P2 guesses A1 and hits, boards and state is updated *)
let game_result_4a = State.p2_guess [('A',1)] game_state_3
let game_state_4a = match game_result_4a with 
  | Legal s -> s
  | _ -> game_state
let game_state_4a_p1_p_board = (State.p1_player_board game_state_4a)
let game_state_4a_p1_e_board = (State.p1_enemy_board game_state_4a)
let game_state_4a_p2_p_board = (State.p2_player_board game_state_4a)
let game_state_4a_p2_e_board = (State.p2_enemy_board game_state_4a)

(** P2 guesses F1 and misses, boards and state is updated *)
let game_result_4b = State.p2_guess [('F',1)] game_state_3
let game_state_4b = match game_result_4b with 
  | Legal s -> s
  | _ -> game_state
let game_state_4b_p1_p_board = (State.p1_player_board game_state_4b)
let game_state_4b_p1_e_board = (State.p1_enemy_board game_state_4b)
let game_state_4b_p2_p_board = (State.p2_player_board game_state_4b)
let game_state_4b_p2_e_board = (State.p2_enemy_board game_state_4b)

(** P1 guesses B3 and hits, boards and state is updated *)
let game_result_5a = State.p1_guess [('B',3)] game_state_4a
let game_state_5a = match game_result_5a with 
  | Legal s -> s
  | _ -> game_state
let game_state_5a_p2_p_board = (State.p2_player_board game_state_5a)
let game_state_5a_p2_e_board = (State.p2_enemy_board game_state_5a)
let game_state_5a_p1_p_board = (State.p1_player_board game_state_5a)
let game_state_5a_p1_e_board = (State.p1_enemy_board game_state_5a)

(** P1 guess G10 and misses, boards and state is updated *)
let game_result_5b = State.p1_guess [('G',10)] game_state_4a
let game_state_5b = match game_result_5b with 
  | Legal s -> s
  | _ -> game_state
let game_state_5b_p2_p_board = (State.p2_player_board game_state_5b)
let game_state_5b_p2_e_board = (State.p2_enemy_board game_state_5b)
let game_state_5b_p1_p_board = (State.p1_player_board game_state_5b)
let game_state_5b_p1_e_board = (State.p1_enemy_board game_state_5b)

(** P2 guesses A2 and hits and sinks the ship, boards and state is updated *)
let game_result_6 = State.p2_guess [('A',2)] game_state_5a
let game_state_6 = match game_result_6 with 
  | Legal s -> s
  | _ -> game_state
let game_state_6_p1_p_board = (State.p1_player_board game_state_6)
let game_state_6_p1_e_board = (State.p1_enemy_board game_state_6)
let game_state_6_p2_p_board = (State.p2_player_board game_state_6)
let game_state_6_p2_e_board = (State.p2_enemy_board game_state_6)

(** P1 guess B2 and hits *)
let game_result_7 = State.p1_guess [('B',2)] game_state_6
let game_state_7 = match game_result_7 with 
  | Legal s -> s
  | _ -> game_state
let game_state_7_p1_p_board = (State.p1_player_board game_state_7)
let game_state_7_p1_e_board = (State.p1_enemy_board game_state_7)
let game_state_7_p2_p_board = (State.p2_player_board game_state_7)
let game_state_7_p2_e_board = (State.p2_enemy_board game_state_7)

(** P1 guess B1 and hits and sinks the ship, state is updated *)
let game_result_8 = State.p1_guess [('B',1)] game_state_7
let game_state_8 = match game_result_8 with 
  | Legal s -> s
  | _ -> game_state
let game_state_8_p1_p_board = (State.p1_player_board game_state_8)
let game_state_8_p1_e_board = (State.p1_enemy_board game_state_8)
let game_state_8_p2_p_board = (State.p2_player_board game_state_8)
let game_state_8_p2_e_board = (State.p2_enemy_board game_state_8)



let state_tests =
  [
    make_p1_enemy_board_test "Empty Player 1 Enemy Board" game_state guessBoard;
    make_p1_player_board_test "Empty Player 1 Player Board" 
      game_state shipBoard;
    make_p2_enemy_board_test "Empty Player 2 Enemy Board" game_state guessBoard;
    make_p2_player_board_test "Empty Player 2 Player Board" game_state 
      shipBoard;

    make_p1_place_test "Place one ship test for P1"  (('A', 1),('A', 2)) 
      game_state game_result_2;
    make_p1_enemy_board_test "One ship for P1" game_state_2 
      game_state_2_p1_e_board;
    make_p1_player_board_test "One ship for P1" game_state_2 
      game_state_2_p1_p_board;
    make_p2_enemy_board_test "One ship for P1" game_state_2 
      game_state_2_p2_e_board;
    make_p2_player_board_test "One ship for P1" game_state_2 
      game_state_2_p2_p_board;
    make_p2_player_board_test "One ship for P1, P2 player board still empty" 
      game_state_2 empty_ship_board;
    make_p1_enemy_board_test "One ship for P1, P1 enemy board still empty " 
      game_state_2 empty_guess_board;
  ]

let board_tests =
  [
    "Testing that place_hit on previous state's board equals next state's board" 
    >:: (fun _ -> assert_equal (true) (place_hit game_state_3_p1_p_board 
                                         ('A', 1) = game_state_4a_p1_p_board));

    "Testing that place_hit on previous state's board equals next state's board" 
    >:: (fun _ -> assert_equal (true) (place_miss game_state_3_p1_p_board 
                                         ('F', 1) = game_state_4b_p1_p_board));


    make_is_ship_sunk_test "A1 A2 ship on P1 player board is sunk" 
      game_state_6_p1_p_board ('A', 1) (true);

    make_is_ship_sunk_test "B1 B3 ship on P2 player board is not hit and not sunk" 
      game_state_3_p2_p_board ('B', 1) (false);

    make_is_ship_sunk_test "B1 B3 ship on P2 player board is hit but not sunk" 
      game_state_6_p2_p_board ('B', 3) (false);

    make_is_ship_sunk_test 
      "A ship is not placed on P2 player board at G8, so it is not sunk" 
      game_state_6_p2_p_board ('G', 8) (false);
  ]

let command_tests =
  [
    make_p2_guess_test "P2 guess hits on P1 board" [('A', 1)] 
      game_state_3 game_result_4a;
    make_p1_player_board_test "One hit on P1 player board" 
      game_state_4a game_state_4a_p1_p_board;
    make_p1_enemy_board_test "No change to P1 enemy board" 
      game_state_4a game_state_4a_p1_e_board;
    make_p1_enemy_board_test 
      "P1 enemy board will be the same as it was in previous state" 
      game_state_4a game_state_3_p1_e_board;
    make_p2_enemy_board_test "One hit on P2 enemy board" 
      game_state_4a game_state_4a_p2_e_board;
    make_p2_player_board_test "No change on P2 player board"
      game_state_4a game_state_4a_p2_p_board;
    make_p2_player_board_test 
      "P2 player board will be the same as it was in previous state" 
      game_state_4a game_state_3_p2_p_board;

    make_p2_guess_test "P2 guess misses on P1 board" [('F', 1)] 
      game_state_3 game_result_4b;
    make_p1_player_board_test "One miss on P1 player board" game_state_4b 
      game_state_4b_p1_p_board;
    make_p1_enemy_board_test "No change to P1 enemy board" game_state_4b 
      game_state_4b_p1_e_board;
    make_p2_enemy_board_test "One miss on P2 enemy board" game_state_4b 
      game_state_4b_p2_e_board;
    make_p2_player_board_test "No change on P2 player board" game_state_4b 
      game_state_4b_p2_p_board;

    make_p1_guess_test "P1 guess hits on P2 board" [('B', 3)] game_state_4a 
      game_result_5a;
    make_p2_player_board_test "One hit on P2 player board" game_state_5a 
      game_state_5a_p2_p_board;
    make_p2_enemy_board_test "No change on P2 enemy board" game_state_5a 
      game_state_5a_p2_e_board;
    make_p2_enemy_board_test 
      "P2 enemy board will be the same as it was in previous state" 
      game_state_5a game_state_4a_p2_e_board;
    make_p1_enemy_board_test "One hit one P1 enemy board" game_state_5a 
      game_state_5a_p1_e_board;
    make_p1_player_board_test "No change to P1 player board" game_state_5a 
      game_state_5a_p1_p_board;
    make_p1_player_board_test 
      "P1 player board will be the same as it was in previous state" 
      game_state_5a game_state_4a_p1_p_board;

    make_p1_guess_test "P1 guess misses on P2 board" [('G', 10)] game_state_4a 
      game_result_5b;
    make_p2_player_board_test "One miss on P2 player board" game_state_5b 
      game_state_5b_p2_p_board;
    make_p2_enemy_board_test "No change on P2 enemy board" game_state_5b 
      game_state_5b_p2_e_board;
    make_p1_enemy_board_test "One miss one P1 enemy board" game_state_5b 
      game_state_5b_p1_e_board;
    make_p1_player_board_test "No change to P1 player board" game_state_5b 
      game_state_5b_p1_p_board;

    make_parse_test "Test quit command" "quit" (Quit);
    make_parse_test "Test quit command with spaces" "     quit  " (Quit);
    make_parse_test "Test place command" "place G3 G4" (Place ["G3"; "G4"]);
    make_parse_test "Test place command with spaces" "     place    A2 A5" 
      (Place ["A2"; "A5"]);
    make_parse_test "Test try command" "try J6" (Try ["J6"]);
    make_parse_test "Test try command with spaces" "     try  I5  " 
      (Try ["I5"]);
    make_parse_test "Test rules command" "rules" (Rules);
    make_parse_test "Test rules command with spaces" "     rules  " (Rules);
    make_parse_test "Test score command" "score" (Score);
    make_parse_test "Test score command with spaces" "  score    " (Score);


    make_parse_test_exn "Test empty input" "" Empty;
    make_parse_test_exn "Test malformed input 1" "random text" Malformed;
    make_parse_test_exn "Test malformed input 2" "tr y A1" Malformed;
    make_parse_test_exn "Test malformed input 3" "pl ace A1 A2" Malformed;
    make_parse_test_exn "Test malformed input 3" "r u  l e      s" Malformed;

    make_p1_score_test 
      "Test score for Player 1, in state 6 when Player 1 has sunk no ships" 
      game_state_6 0;
    make_p2_score_test 
      "Test score for Player 2, in state 6 when Player 2 has sunk 1 ship" 
      game_state_6 1;
    make_p1_score_test 
      "Test score for Player 1 in state 5a where its ship is hit but not sunk" 
      game_state_5a 0;
    make_p2_score_test 
      "Test score for Player 2 in state 5a where its ship is hit but not sunk" 
      game_state_5a 0;
    make_p1_score_test 
      "Test score for Player 1, in state 8 when Player 1 has sunk 1 ship" 
      game_state_8 1;
    make_p2_score_test 
      "Test score for Player 2, in state 8 when Player 2 has sunk 1 ship" 
      game_state_8 1;

  ]


let suite =
  "test suite for A2"  >::: List.flatten [
    board_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
