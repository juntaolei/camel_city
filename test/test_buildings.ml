open OUnit2
open Lib.Buildings

let water = new_resource "water" 1

let electricity_lst = [ new_resource "electricity" 8 ]

let resource_tests =
  [
    ( "resource name is \"water\"" >:: fun _ ->
      assert_equal (resource_name water) "water" );
    ( "resource amount of \"water\" is 1" >:: fun _ ->
      assert_equal (resource_amount water) 1 );
  ]

let building_tests =
  [
    ( "resource output for oats plantation is oat" >:: fun _ ->
      assert_equal (oats_plantation |> output |> resource_name) "oat" );
    ( "amount of resource output for oats plantation is 10" >:: fun _ ->
      assert_equal (oats_plantation |> output |> resource_amount) 10 );
    (* ( "tax amount of barrack is 0" >:: fun _ -> assert_equal
       (tax_amount barrack) 0 ); *)
    (* ( "amount of electricity required for mine is 8" >:: fun _ ->
       assert_equal (resource_dependency mine "electricity") 8 ); (
       "insufficient electricity given for mine" >:: fun _ ->
       assert_equal (resource_sufficiency_check mine (new_resource
       "electricity" 1)) None ); *)
  ]

let test_suite = List.flatten [ resource_tests; building_tests ]
