open OUnit2
open Lib.Buildings

let water = new_resource "water" 1

(*let electricity_lst = [ new_resource "electricity" 8 ]*)
let road_1  = new_road 10 1 2

let planation_bld = new_building "oat plantation" 10 5 5 "oat" 6 7 0 ""

let resource_tests =
  [
    ( "resource name is \"water\"" >:: fun _ ->
      assert_equal (resource_name water) "water" );
    ( "resource amount of \"water\" is 1" >:: fun _ ->
      assert_equal (resource_amount water) 1 );
  ]

let building_tests =
  [
    ( "building name of oat plantation is \"oat plantation\"" >:: fun _ ->
      assert_equal (building_name planation_bld) "oat plantation" );
    ( "cost of oat plantation is 10" >:: fun _ ->
      assert_equal (cost planation_bld) 10 );
    ( "maintenance of oat plantation is 5" >:: fun _ ->
      assert_equal (maintenance planation_bld) 5 );
    ( "resource output of oat plantation is \"oat\"" >:: fun _ ->
      assert_equal (resource_name (output planation_bld)) "oat");
    ( "amount of resource output of oat plantation is 5" >:: fun _ ->
      assert_equal (resource_amount (output planation_bld)) 5);
    ( "income of oat plantation is 6" >:: fun _ ->
      assert_equal (income planation_bld) 6 );
    ( "defense of oat plantation is 7" >:: fun _ ->
      assert_equal (defense planation_bld) 7 )

  ]

let road_tests = 
  [
    ( "cost of road_1 is 10" >:: fun _ ->
      assert_equal (cost_rd road_1) 10 )
  ]

let test_suite = List.flatten [ resource_tests; building_tests; road_tests ]
