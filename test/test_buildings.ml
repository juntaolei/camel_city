open OUnit2
open Lib.Buildings

(* This test suite covers the most basic commands in [buildings.ml]
   including functions for resources, roads, and buildings. *)
let water = new_resource "water" 1

let road = new_road 1 2

let plantation_building =
  new_building "oat plantation" 10 5 ("oat", 5) 6 7 [] 0 0 0 0 false

let mine_building =
  new_building "mine" 30 30 ("iron", 20) 5 4 [ ("electricity", 3) ] 0 0
    0 0 false

let electricity_resource = new_resource "electricity" 3

let resource_tests =
  [
    ( "resource name is \"water\"" >:: fun _ ->
      assert_equal (resource_name water) "water" );
    ( "resource amount of \"water\" is 1" >:: fun _ ->
      assert_equal (resource_amount water) 1 );
  ]

let building_tests =
  [
    ( "building name of oat plantation is \"oat plantation\""
    >:: fun _ -> assert_equal plantation_building.name "oat plantation"
    );
    ( "cost of oat plantation is 10" >:: fun _ ->
      assert_equal plantation_building.cost 10 );
    ( "maintenance of oat plantation is 5" >:: fun _ ->
      assert_equal plantation_building.maintenance 5 );
    ( "resource output of oat plantation is \"oat\"" >:: fun _ ->
      assert_equal (resource_name plantation_building.output) "oat" );
    ( "amount of resource output of oat plantation is 5" >:: fun _ ->
      assert_equal (resource_amount plantation_building.output) 5 );
    ( "income of oat plantation is 6" >:: fun _ ->
      assert_equal plantation_building.income 6 );
    ( "defense of oat plantation is 7" >:: fun _ ->
      assert_equal plantation_building.defense 7 );
    ( "plantation has no resource dependency" >:: fun _ ->
      assert_equal plantation_building.resource_dependency [] );
    ( "building name of mine is \"mine\"" >:: fun _ ->
      assert_equal mine_building.name "mine" );
    ("cost of min is 30" >:: fun _ -> assert_equal mine_building.cost 30);
    ( "mine requires 3 units of electricity" >:: fun _ ->
      assert_equal mine_building.resource_dependency
        [ electricity_resource ] );
  ]

let road_tests =
  [
    ("x coordinate of road is 1" >:: fun _ -> assert_equal (fst road) 1);
    ("y coordinate of road is 2" >:: fun _ -> assert_equal (snd road) 2);
  ]

let test_suite =
  List.flatten [ resource_tests; building_tests; road_tests ]
