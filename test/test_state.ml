open OUnit2
open Lib.State
open Lib.Buildings

let state = new_state "test_state.json" 1000 900 10 128 64

let state_1 = from_file "map_1.json"

let expected_cell_array =
  let empty = Array.make_matrix 3 3 None in
  empty.(0).(0) <-
    Building (new_building "mine" 0 0 1 "iron" 0 0 5 "electricity");
  empty.(0).(1) <- Building (new_building "house" 0 0 0 "" 0 0 0 "");
  empty.(2).(0) <-
    Building (new_building "oats_plantation" 0 0 10 "oat" 0 0 0 "");
  empty

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

let rec string_of_lst acc = function
  | [] -> acc
  | h :: t -> string_of_lst (acc ^ str_of_cell h ^ ",") t

let rec string_of_lst_lst acc = function
  | [] -> acc
  | h :: t -> string_of_lst_lst ("[" ^ string_of_lst "" h ^ "]" ^ acc) t

let canvas_size_test name st expected : test =
  name >:: fun _ ->
  assert_equal expected (canvas_size st) ~printer:(fun (a, b) ->
      string_of_int a ^ string_of_int b)

let cell_size_test name st expected : test =
  name >:: fun _ ->
  assert_equal expected (cell_size st) ~printer:(fun (a, b) ->
      string_of_int a ^ string_of_int b)

let map_length_test name st expected : test =
  name >:: fun _ ->
  assert_equal expected (map_length st) ~printer:string_of_int

let population_test name st expected : test =
  name >:: fun _ ->
  assert_equal expected (population st) ~printer:string_of_int

let cell_test name st expected : test =
  name >:: fun _ ->
  assert_equal expected (cells st) ~printer:(fun x ->
      string_of_lst_lst "" (lst_of_array_array x))

let stockpile_test name st expected : test =
  name >:: fun _ -> assert_equal expected (stockpile st)

let state_test =
  [
    canvas_size_test "canvas size of state is 1000 by 900" state
      (1000, 900);
    map_length_test "map length of state is 10" state 10;
    cell_size_test "cell size of state is 128 by 64" state (128, 64);
    canvas_size_test "canvas size of state_1 is 300 by 300" state_1
      (300, 300);
    map_length_test "map length of state_1 is 3" state_1 3;
    cell_size_test "cell size of state_1 is 100 by 100" state_1
      (100, 100);
    population_test "population testing" state_1 20;
    cell_test "cell test for state_1" state_1 expected_cell_array;
  ]

let test_suite = List.flatten [ state_test ]
