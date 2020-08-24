/*
 * Advent of Code 2017 Day08
 */
open Belt;

let input =
  Node_fs.readFileAsUtf8Sync("input/day08")->Js.String2.split("\n");

let compute = (map_of_reg, reg, value, op) => {
  let prev_value = Map.String.get(map_of_reg, reg);
  let value' = int_of_string(value);
  switch (prev_value) {
  | None =>
    switch (op) {
    | "inc" => Map.String.set(map_of_reg, reg, 0 + value')
    | "dec" => Map.String.set(map_of_reg, reg, 0 - value')
    | _ => map_of_reg
    }
  | Some(p_v) =>
    switch (op) {
    | "inc" => Map.String.set(map_of_reg, reg, p_v + value')
    | "dec" => Map.String.set(map_of_reg, reg, p_v - value')
    | _ => map_of_reg
    }
  };
};

let check_condition = (map_of_reg, reg, value, op) => {
  let cur_value = Map.String.get(map_of_reg, reg);
  let value' = int_of_string(value);
  switch (cur_value) {
  | None =>
    let new_map = Map.String.set(map_of_reg, reg, 0);
    switch (op) {
    | ">" => 0 > value' ? Ok(new_map) : Error(new_map)
    | ">=" => 0 >= value' ? Ok(new_map) : Error(new_map)
    | "<" => 0 < value' ? Ok(new_map) : Error(new_map)
    | "<=" => 0 <= value' ? Ok(new_map) : Error(new_map)
    | "==" => 0 == value' ? Ok(new_map) : Error(new_map)
    | "!=" => 0 != value' ? Ok(new_map) : Error(new_map)
    | _ => Error(new_map)
    };
  | Some(c_v) =>
    switch (op) {
    | ">" => c_v > value' ? Ok(map_of_reg) : Error(map_of_reg)
    | ">=" => c_v >= value' ? Ok(map_of_reg) : Error(map_of_reg)
    | "<" => c_v < value' ? Ok(map_of_reg) : Error(map_of_reg)
    | "<=" => c_v <= value' ? Ok(map_of_reg) : Error(map_of_reg)
    | "==" => c_v == value' ? Ok(map_of_reg) : Error(map_of_reg)
    | "!=" => c_v != value' ? Ok(map_of_reg) : Error(map_of_reg)
    | _ => Error(map_of_reg)
    }
  };
};

let get_peak = map => {
  Map.String.reduce(map, 0, (acc, _, v) => {acc > v ? acc : v});
};

let parse = instructions => {
  let map_of_reg = [||]->Map.String.fromArray;
  let list_of_instructions = List.fromArray(instructions);

  let rec parse' = (map_of_reg', instructions', val_of_peak') => {
    let new_peak = map_of_reg'->get_peak;
    let next_peak = new_peak > val_of_peak' ? new_peak : val_of_peak';

    switch (instructions') {
    | [first, ...rest] =>
      let splitted = Js.String2.split(first, " ");

      let reg_of_computation = Option.getExn(splitted[0]);
      let op_of_computation = Option.getExn(splitted[1]);
      let val_of_computation = Option.getExn(splitted[2]);
      let reg_of_condition = Option.getExn(splitted[4]);
      let op_of_condition = Option.getExn(splitted[5]);
      let val_of_condition = Option.getExn(splitted[6]);

      switch (
        check_condition(
          map_of_reg',
          reg_of_condition,
          val_of_condition,
          op_of_condition,
        )
      ) {
      | Ok(new_map) =>
        let result =
          compute(
            new_map,
            reg_of_computation,
            val_of_computation,
            op_of_computation,
          );
        parse'(result, rest, next_peak);
      | Error(new_map) => parse'(new_map, rest, next_peak)
      };
    | [] => (map_of_reg', next_peak)
    };
  };

  parse'(map_of_reg, list_of_instructions, 0);
};

let result = input->parse;

let get_part1 = result => {
  let (final_map, _) = result;
  final_map->Map.String.reduce(0, (acc, _, v) => {acc > v ? acc : v});
};

let get_part2 = result => {
  let (_, peak) = result;
  peak;
};

result->get_part1->Js.log;
result->get_part2->Js.log;
