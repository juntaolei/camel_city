
open buildings
let m = 1

type residential = {
  house = house list
}
type resource_generating = {
  oats_plantations = oats_plantation list


}
type industrial = {
  power_plants = power_plant list


}
type military = {

}

type buildings = {
  residentials = residential 
  resource_generatings = resource_generating 
  industrials =  industrial 
  militarys  = military 

}

type game_state = {
  tick: int
  buildings : buildings 
  resources = resource list
  roads = road list
  money = int

}
let rec add_resources buildings_catagorie = 

let rec minus_resources buildings = 

let update_resources resources  = 
  add_resources


let update_money a =

let build = 
let rec update_game_state game_state = 
  match game_state.tick with
  | > 500 ->
  | _ ->  let money = update_money in 
    let new_tick = game_state.tick + 1 in
    new_game_state = {
      tick = new_tick

    }
    update_game_state 








