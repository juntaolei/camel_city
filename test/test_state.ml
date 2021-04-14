open OUnit2
open Lib.State

let state = new_state 1000 900 10 128 64

let state_test =
  [
    ( "canvas size of state is 1000 by 900" >:: fun _ ->
      assert_equal (canvas_size state) (1000, 900) );
    ( "map length of state is 10" >:: fun _ ->
      assert_equal (map_length state) 10 );
    ( "cell size of state is 128 by 64" >:: fun _ ->
      assert_equal (cell_size state) (128, 64) );
  ]

let test_suite = List.flatten [ state_test ]
