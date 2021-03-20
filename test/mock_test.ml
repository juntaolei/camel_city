open OUnit2

let mock_test = [ ("mock test" >:: fun _ -> assert_equal (1 + 2) 3) ]
