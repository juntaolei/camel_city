(** This test file contains Ounit tests of the basic functions for the
    states and buildings module that involves creating and updating
    resource, building, and state objects. We used glass box testing and
    wrote test cases for the default case and different combinations of
    parameters to account for different possible situations. We also
    created external files and tested if the program can read from json
    files correctly. As for the progression of the game itself, since
    random events are involved we use manual testing instead by
    displaying relevant parameters in the console and observe if the
    program handles situatinos correctly, for example, adds or subtracts
    a certain resource, or removes buildings from the map. *)

open OUnit2

let suite =
  "test suite for camel city"
  >::: List.flatten [ Test_cells.test_suite; Test_state.test_suite ]

let _ = run_test_tt_main suite
