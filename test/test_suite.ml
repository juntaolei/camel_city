open OUnit2

let suite =
  "test suite for camel city"
  >::: List.flatten [ Test_buildings.test_suite; Test_state.test_suite ]

let _ = run_test_tt_main suite
