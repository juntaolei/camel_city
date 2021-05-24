(* This test suite covers the most basic commands in [state.ml]
   including functions for creating states, placing cells and buildings. *)
open Lib.Cells
open Lib.State
open OUnit2

let read_file_as_string filename =
  let channel = open_in filename in
  let content =
    really_input_string channel (in_channel_length channel)
  in
  close_in channel;
  content

(** [duplicate_state s] is the state duplicated from state [s], for the
    purpose of testing state updates. *)
let duplicate_state (s : state) =
  new_state ~tick:s.tick ~housing_capacity:s.housing_capacity
    ~population:s.population ~unemployed:s.unemployed ~food:s.food
    ~deficit_counter:s.deficit_counter
    ~starvation_counter:s.starvation_counter ~is_paused:s.is_paused
    ~is_game_over:s.is_game_over ~condition:s.condition
    ~is_final_building_placed:s.is_final_building_placed
    (fst s.canvas_size) (snd s.canvas_size) s.map_length
    (fst s.cell_size) (snd s.cell_size)

(** [state_0] is the default state created with the [new_state]
    function. *)
let state_0 = new_state 1000 900 10 128 64

(** [state_1] is the state extracted from file [map1.json]. *)
let state_1 = from_string (read_file_as_string "map_1.json")

(** [state_1_1] is the state of [state_1] after placing an empty cell. *)
let state_1_1 = duplicate_state state_1

(** [state_1_2] is the state of [state_1] after placing a new oat
    plantation. *)
let state_1_2 = duplicate_state state_1

(** [state_2] is the state of [state_1] after placing a new mine. *)
let state_2 = duplicate_state state_1

(** [transfer_cells s n] transfers the content of cells from [n] to [s]. *)
let transfer_cells (s : state) (n : state) =
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j cell ->
          match cell with
          | Building building -> n.cells.(i).(j) <- Building building
          | Road r -> n.cells.(i).(j) <- Road r
          | _ -> n.cells.(i).(j) <- None)
        row)
    s.cells

(** [plantation] is a building for testing purposes *)
let plantation =
  new_building "oats_plantation" 10 5 ("food", 5) 6 7 [] 0 0 false

(** [mine] is a building for testing purposes *)
let mine = new_building "mine" 100 5 ("iron", 5) 6 7 [] 2 0 false

(** [place_init] initializes the states defined above. *)
let place_init =
  state_1_1.stockpile <- state_1.stockpile;
  transfer_cells state_1 state_1_1;
  state_1_2.stockpile <- state_1.stockpile;
  transfer_cells state_1 state_1_2;
  state_2.stockpile <- List.rev state_1.stockpile;
  state_2.buildings <- [ mine ];
  transfer_cells state_1 state_2;
  place_cell state_1_1 (Building plantation) 6 1;
  place_cell state_1_2 None 3 0;
  place_building state_2 "mine" 2 2

(** [expected_cell_array_0]is the expected cell array array extracted
    from state_0. *)
let expected_cell_array_0 = Array.make_matrix 10 10 None

(** [expected_cell_array_1] is the expected cell array array extracted
    from state_1 in file "map_1.json". *)
let expected_cell_array_1 =
  let empty = Array.make_matrix 9 9 None in
  empty.(3).(0) <-
    Building
      (new_building "power_plant" 200 0 ("electricity", 5) 5 3 [] 5 0
         false);
  empty.(4).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(5).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(6).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(7).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(7).(1) <-
    Building
      (new_building "oats_plantation" 50 0 ("food", 10) 5 1 [] 3 0 false);
  empty.(8).(0) <-
    Building
      (new_building "oats_plantation" 50 0 ("food", 10) 5 1 [] 3 0 false);
  empty

(** [expected_cell_array_1] is the expected cell array array extracted
    from state_1_1 by placing an oats plantation cell. *)
let expected_cell_array_1_1 =
  let empty = Array.make_matrix 9 9 None in
  empty.(3).(0) <-
    Building
      (new_building "power_plant" 200 0 ("electricity", 5) 5 3 [] 5 0
         false);
  empty.(4).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(5).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(6).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(6).(1) <-
    Building
      (new_building "oats_plantation" 10 5 ("food", 5) 6 7 [] 0 0 false);
  empty.(7).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(7).(1) <-
    Building
      (new_building "oats_plantation" 50 0 ("food", 10) 5 1 [] 3 0 false);
  empty.(8).(0) <-
    Building
      (new_building "oats_plantation" 50 0 ("food", 10) 5 1 [] 3 0 false);
  empty

(** [expected_cell_array_1_2] is the expected cell array array extracted
    from state_1_2 by placing an empty cell. *)
let expected_cell_array_1_2 =
  let empty = Array.make_matrix 9 9 None in
  empty.(4).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(5).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(6).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(7).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(7).(1) <-
    Building
      (new_building "oats_plantation" 50 0 ("food", 10) 5 1 [] 3 0 false);
  empty.(8).(0) <-
    Building
      (new_building "oats_plantation" 50 0 ("food", 10) 5 1 [] 3 0 false);
  empty

(** [expected_cell_array_2] is the expected cell array array extracted
    from state_2 by placing a new mine. *)
let expected_cell_array_2 =
  let empty = Array.make_matrix 9 9 None in
  empty.(2).(2) <-
    Building (new_building "mine" 100 5 ("iron", 5) 6 7 [] 2 0 false);
  empty.(3).(0) <-
    Building
      (new_building "power_plant" 200 0 ("electricity", 5) 5 3 [] 5 0
         false);
  empty.(4).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(5).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(6).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(7).(0) <-
    Building (new_building "house" 50 0 ("", 0) 10 1 [] 0 5 false);
  empty.(7).(1) <-
    Building
      (new_building "oats_plantation" 50 0 ("food", 10) 5 1 [] 3 0 false);
  empty.(8).(0) <-
    Building
      (new_building "oats_plantation" 50 0 ("food", 10) 5 1 [] 3 0 false);
  empty

(** [init_stockpile] is the default stockpile. *)
let init_stockpile =
  [
    new_resource "money" 200;
    new_resource "electricity" 0;
    new_resource "food" 100;
    new_resource "iron" 0;
    new_resource "coal" 0;
    new_resource "steel" 0;
    new_resource "canned oats" 0;
  ]

(** [expected_stockpile_1] is the expected stockpile extracted from file
    "map_1.json". *)
let expected_stockpile_1 =
  [
    new_resource "money" 605;
    new_resource "electricity" 5;
    new_resource "food" 124;
    new_resource "iron" 0;
    new_resource "coal" 0;
    new_resource "steel" 0;
    new_resource "canned oats" 0;
  ]

(** [expected_stockpile_2] is the stockpile of state_1 after placing a
    new oat plantation. *)
let expected_stockpile_2 =
  [
    new_resource "money" 505;
    new_resource "electricity" 5;
    new_resource "food" 124;
    new_resource "iron" 0;
    new_resource "coal" 0;
    new_resource "steel" 0;
    new_resource "canned oats" 0;
  ]

(** [lst_of_array_array a] is the cell list list represented by the cell
    array array [a]. *)
let lst_of_array_array a =
  let acc = ref [] in
  for i = 0 to Array.length a - 1 do
    let row = Array.get a i in
    let acc_r = ref [] in
    for j = 0 to Array.length row - 1 do
      acc_r := Array.get row j :: !acc_r
    done;
    acc := !acc_r :: !acc
  done;
  !acc

(** [string_of_lst acc lst] is the string representation of cell list
    [lst]. *)
let rec string_of_lst acc = function
  | [] -> acc
  | h :: t -> string_of_lst (acc ^ str_of_cell h ^ ",") t

(** [string_of_lst acc lst] is the string representation of cell list
    list [lst]. *)
let rec string_of_lst_lst acc = function
  | [] -> acc
  | h :: t -> string_of_lst_lst ("[" ^ string_of_lst "" h ^ "]" ^ acc) t

(** [string_of_stockpile acc sp] is the string representation of
    stockpile [sp]. *)
let rec string_of_stockpile acc = function
  | [] -> acc
  | h :: t ->
      string_of_stockpile
        (acc ^ resource_name h ^ ":" ^ string_of_int (resource_amount h))
        t

(** [canvas_size_test n s e] tests if the canvas size of [s] equals [e]. *)
let canvas_size_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.canvas_size ~printer:(fun (a, b) ->
      string_of_int a ^ string_of_int b)

(** [cell_size_test n s e] tests if the cell size of [s] equals [e]. *)
let cell_size_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.cell_size ~printer:(fun (a, b) ->
      string_of_int a ^ string_of_int b)

(** [map_length_test n s e] tests if the mape length of [s] equals [e]. *)
let map_length_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.map_length ~printer:string_of_int

(** [population_test n s e] tests if the population size of [s] equals
    [e]. *)
let population_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.population ~printer:string_of_int

(** [unemployment_test n s e] tests if the unemployment population size
    of [s] equals [e]. *)
let unemployment_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.unemployed ~printer:string_of_int

(** [deficit_counter_test n s e] test the value of the deficit counter
    of [s]. *)
let deficit_counter_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.deficit_counter ~printer:string_of_int

(** [deficit_counter_test n s e] test the value of the deficit counter
    of [s]. *)
let starvation_counter_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.starvation_counter ~printer:string_of_int

(** [cell_test n s e] tests if the cell array array of [s] equals [e]. *)
let cell_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.cells ~printer:(fun x ->
      string_of_lst_lst "" (lst_of_array_array x))

(** [stockpile_test n s e] tests if the stockpile of [s] equals [e]. *)
let stockpile_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.stockpile ~printer:(fun x ->
      string_of_stockpile "" x)

let state_test =
  [
    (* default new_state *)
    canvas_size_test "canvas size of state_0 is 1000 by 900" state_0
      (1000, 900);
    map_length_test "map length of state_0 is 10" state_0 10;
    cell_size_test "cell size of state_0 is 128 by 64" state_0 (128, 64);
    population_test "population state_0 is 0" state_0 0;
    unemployment_test "unemployment of state_0" state_0 0;
    deficit_counter_test "deficit counter of state_0" state_0 0;
    starvation_counter_test "starvation counter of state_0" state_0 0;
    stockpile_test "stockpile of state_0 is empty" state_0
      init_stockpile;
    cell_test "cell test for state_0" state_0 expected_cell_array_0;
    (* state_1 *)
    canvas_size_test "canvas size of state_1 is 1200 by 750" state_1
      (1200, 750);
    map_length_test "map length of state_1 is 9" state_1 9;
    cell_size_test "cell size of state_1 is 128 by 64" state_1 (128, 64);
    population_test "population of state_1 is 16" state_1 16;
    unemployment_test "unemployment of state_1" state_1 5;
    deficit_counter_test "deficit counter of state_1" state_1 0;
    starvation_counter_test "starvation counter of state_1" state_1 0;
    stockpile_test "stockpile of state_1" state_1 expected_stockpile_1;
    cell_test "cell test for state_1" state_1 expected_cell_array_1;
    (* placing cells *)

    (* state_1_1 *)
    canvas_size_test "canvas size of state_1_1 is 1200 by 750" state_1_1
      (1200, 750);
    map_length_test "map length of state_1_1 is 9" state_1_1 9;
    cell_size_test "cell size of state_1_1 is 128 by 64" state_1_1
      (128, 64);
    unemployment_test "unemployment of state_1_1" state_1_1 5;
    stockpile_test "stockpile of state_1_1" state_1_1
      expected_stockpile_1;
    cell_test "cell test for state_1_1" state_1_1
      expected_cell_array_1_1;
    (* state_1_2 *)
    canvas_size_test "canvas size of state_1_2 is 1200 by 750" state_1_2
      (1200, 750);
    map_length_test "map length of state_1_2 is 9" state_1_2 9;
    cell_size_test "cell size of state_1_2 is 128 by 64" state_1_2
      (128, 64);
    unemployment_test "unemployment of state_1_2" state_1_2 5;
    stockpile_test "stockpile of state_1_2" state_1_2
      expected_stockpile_1;
    cell_test "cell test for state_1_2" state_1_2
      expected_cell_array_1_2;
    (* placing buildings, involves checking conditions *)
    canvas_size_test "canvas size of state_2 is 1200 by 750" state_2
      (1200, 750);
    map_length_test "map length of state_2 is 9" state_2 9;
    unemployment_test "unemployment of state_2" state_2 3;
    stockpile_test "stockpile of state_2" state_2 expected_stockpile_2;
    cell_test "cell test for state_2" state_2 expected_cell_array_2;
  ]

let test_suite = List.flatten [ state_test ]
