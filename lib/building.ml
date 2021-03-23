type resource = {
  amount : int;
  name : string;
}

type road = {
  cost : int;
  x_cord : int;
  y_cord : int;
}

type building = {
  name : string;
  cost : int;
  maintenance : int;
  output : resource;
  tax : int;
  defense : int;
  building_dependency : building list;
  resource_dependency : resource list;
}

type camel = { food : int}

let house =
  {
    name = "house";
    cost = 0;
    maintenance = 0;
    output = { amount = 0; name = "" };
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let oat = { amount = 0; name = "oat" }

let oats_plantation =
  {
    name = "oats planation";
    cost = 0;
    maintenance = 0;
    output = oat;
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let electricity = { amount = 0; name = "electricity" }

let power_plant =
  {
    name = "power_plant";
    cost = 0;
    maintenance = 0;
    output = electricity;
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let iron = { amount = 0; name = "iron" }

let mine =
  {
    name = "mine";
    cost = 0;
    maintenance = 0;
    output = iron;
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let money = { amount = 0; name = "money" }

let barrack =
  {
    name = "barrack";
    cost = 0;
    maintenance = 0;
    output = { amount = 0; name = "" };
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

  (*
let place_cell conf cell x_coord y_coord =
    List.mapi (fun i x -> if i = x_coord then 
      List.mapi (fun i y -> if i = y_coord then cell else y) else x) conf.cell

(* helper functions *)
(* summing tax for a cell list*)
let rec sum_tax = function
  | [] -> 0
  | h::t -> match h with
            | Building of building ->
              building.tax + sum_tax t
            | _ -> sum_tax t

let rec tax_amount = function
  | [] -> 0
  | h :: t -> (sum_tax h) + tax_amount t
*)