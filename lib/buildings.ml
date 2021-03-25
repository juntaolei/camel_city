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
  input : resource; 
  output : resource;
  tax : int;
  defense : int;
  building_dependency : building list;
  resource_dependency : resource list;
}

type camel = { food : int}

let oat = { amount = 0; name = "oat" }

let electricity = { amount = 0; name = "electricity" }

let iron = { amount = 0; name = "iron" }

let money = { amount = 0; name = "money" }

let house =
  {
    name = "house";
    cost = 0;
    maintenance = 0;
    input = { amount = 0; name = "" };
    output = { amount = 0; name = "" };
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let oats_plantation =
  {
    name = "oats planation";
    cost = 0;
    maintenance = 0;
    input = { amount = 0; name = "" };
    output = { amount = 10; name = "oat" };
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let power_plant =
  {
    name = "power_plant";
    cost = 0;
    maintenance = 0;
    input = { amount = 0; name = "" };
    output = { amount = 5; name = "electricity" };
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let mine =
  {
    name = "mine";
    cost = 0;
    maintenance = 0;
    input = { amount = 8; name = "electricity" };
    output = { amount = 1; name = "iron" };
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let barrack =
  {
    name = "barrack";
    cost = 0;
    maintenance = 0;
    input = { amount = 0; name = "" };
    output = { amount = 0; name = "" };
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let new_resource num str = {amount = num; name = str}

let get_input bld resource_name = 
  if bld.input.name = resource_name then bld.input.amount else 0

let get_output bld resource_name = 
  if bld.output.name = resource_name then bld.output.amount else 0

let get_tax bld = bld.tax

let input_resource_check (bld : building) (res : resource) =
  if (get_input bld res.name) <= res.amount then 
    Some {name = res.name; amount = res.amount - (get_input bld res.name)}
  else None
