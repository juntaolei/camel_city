open Buildings
open Types

type buildings = {
  residential = building list 
  resource_generating = building list 
  industrial = building list  
  military = building list  

}

type game_state = {
  tick: int
  buildings : buildings
  resources = resource_stockpile
  roads = road list
  money = int
  pop = int

}


let rec add_resources buildings_catagorie resources = 
  match building_list with 
  | [] -> 0
  | h::t -> h.output.amount extract_resource t resources

let rec minus_resources_helper resource_dependency resources = 
  match resource_dependency with
  | [] -> 0
  | h::t -> h.amount minus_resources_helper t resources

let rec minus_resources buildings_catagorie resources= 
  match building_list with 
  | [] -> 0
  | h::t -> h.output.resource_dependency 

let update_resources resources game_state = 
  add_resources game_state.buildings.residential
  minus_resources game_state.buildings.residential
  add_resources game_state.buildings.residential


let update_money a =

let rec update_game_state game_state = 
  match game_state.tick with
  | > 500 ->
  | _ ->  let money = update_money in 
    let new_tick = game_state.tick + 1 in
    new_game_state = {
      tick = new_tick

    }
    update_game_state new_game_state



let rec add_resources state update = failwith "Unimplemented"

let rec sub_resources state update = failwith "Unimplemented"

let update_resources state update = failwith "Unimplemented"

let update_money state update = failwith "Unimplemented"

let next_state game_state update =
  (* match game_state.tick with | > 500 -> | _ -> let money =
     update_money in let new_tick = game_state.tick + 1 in
     new_game_state = { tick = new_tick

     } update_game_state *)
  failwith "Unimplemented"
