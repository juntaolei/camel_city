open OUnit2

let suite =
  "test suite for camel city" >::: List.flatten [ Mock_test.mock_test ]

let _ = run_test_tt_main suite
