open Buildings
open Types

type update = { stockpile : stockpile }

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
