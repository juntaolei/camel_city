(* This test suite covers the most basic commands in [state.ml]
   including functions for creating and updating states. *)
open OUnit2
open Lib.State
open Lib.Buildings

let read_file_as_string filename =
  let channel = open_in filename in
  let content =
    really_input_string channel (in_channel_length channel)
  in
  close_in channel;
  content

(** [state_0] is the default state created with the [new_state] function. *)
let state_0 = new_state 1000 900 10 128 64

(** [state_1] is the state extracted from file [map1.json]. *)
let state_1 = from_string (read_file_as_string "map_1.json")

(** [expected_cell_array_0]is the expected cell array array extracted
    from state_0. *)
let expected_cell_array_0 = Array.make_matrix 10 10 None

(** [expected_cell_array_1] is the expected cell array array extracted
    from state_1 in file "map_1.json". *)
let expected_cell_array_1 =
  let empty = Array.make_matrix 9 9 None in
  empty.(3).(0) <-
    Building
      (new_building "power_plant" 200 0 ("electricity", 5) 5 3 [] 5 0 false);
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

let init_stockpile =
  [
    new_resource "money" 200;
    new_resource "electricity" 0;
    new_resource "food" 0;
    new_resource "iron" 0;
    new_resource "coal" 0;
  ]

(** [expected_stockpile_1] is the expected stockpile extracted from file
    "map_1.json". *)
let expected_stockpile_1 =
  [
    new_resource "money" 0;
    new_resource "electricity" 40;
    new_resource "iron" 20;
    new_resource "money" 100;
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

let canvas_size_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.canvas_size ~printer:(fun (a, b) ->
      string_of_int a ^ string_of_int b)

let cell_size_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.cell_size ~printer:(fun (a, b) ->
      string_of_int a ^ string_of_int b)

let map_length_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.map_length ~printer:string_of_int

let population_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.population ~printer:string_of_int

let cell_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected state.cells ~printer:(fun x ->
      string_of_lst_lst "" (lst_of_array_array x))

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
    stockpile_test "stockpile of state_0 is empty" state_0 init_stockpile;
    cell_test "cell test for state_0" state_0 expected_cell_array_0;

    (* state_1 *)
    canvas_size_test "canvas size of state_1 is 1200 by 750" state_1
      (1200, 750);
    map_length_test "map length of state_1 is 9" state_1 9;
    cell_size_test "cell size of state_1 is 128 by 64" state_1
      (128, 64);
    population_test "population of state_1 is 16" state_1 16;
    cell_test "cell test for state_1" state_1 expected_cell_array_1;
  ]

let test_suite = List.flatten [ state_test ]
