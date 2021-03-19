open OUnit2

let mock_test = [ ("mock test" >:: fun _ -> assert_equal (1 + 2) 3) ]

let suite = "test suite for camel city" >::: List.flatten [ mock_test ]

let _ = run_test_tt_main suite
