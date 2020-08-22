/*
 * Advent of Code 2017 Day07
 */
open Belt;

let input =
  Node_fs.readFileAsUtf8Sync("input/day07")->Js.String2.split("\n");

// [|"abb (55)", "baa (34) -> cdd, dee", "cdd (47) -> abb", "dee (33)"|]
// -> Map.String((child, parent)) = [("cdd", "baa"), ("dee", "baa"), ...]
// -> rec find(child -> parent -> child -> parent)
// -> ancestor

let parse = input => {
  Array.map(input, Js.String.split(" -> "))
  ->Array.reduce([||], (acc, item) =>
      switch (item[1]) {
      | None => acc
      | Some(children) =>
        switch (item[0]) {
        | None => acc
        | Some(parent) =>
          let parent_name = Js.String2.split(parent, " ");
          switch (parent_name[0]) {
          | None => acc
          | Some(p_name) =>
            Array.concat(
              acc,
              Js.String2.split(children, ", ")
              ->Array.map(Js.String2.trim)
              ->Array.map(child => (child, p_name)),
            )
          };
        }
      }
    )
  ->Map.String.fromArray;
};

let parse_children = input => {
  Array.map(input, Js.String.split(" -> "))
  ->Array.reduce([||], (acc, item) => {
      switch (item[1]) {
      | Some(_) => acc
      | None =>
        switch (item[0]) {
        | None => acc
        | Some(child) =>
          let c = Js.String2.split(child, " ");
          switch (c[0]) {
          | None => acc
          | Some(name) => Array.concat(acc, [|Js.String.trim(name)|])
          };
        }
      }
    });
};

let parse_weight = input => {
  Array.map(input, Js.String.split(" -> "))
  ->Array.map(item => {
      switch (item[0]) {
      | None => ""
      | Some(program) => program
      }
    })
  ->Array.reduce([||], (acc, item) => {
      item == "" ? acc : Array.concat(acc, [|item|])
    })
  ->Array.map(item => Js.String2.split(item, " "))
  ->Array.reduce([||], (acc, item) => {
      switch (item[1]) {
      | None =>
        switch (item[0]) {
        | None => acc
        | Some(without_weight) =>
          Array.concat(acc, [|(without_weight, 0)|])
        }
      | Some(weight) =>
        switch (item[0]) {
        | None => acc
        | Some(with_weight) =>
          Array.concat(
            acc,
            [|
              (
                with_weight,
                int_of_string(Js.String2.slice(weight, ~from=1, ~to_=-1)),
              ),
            |],
          )
        }
      }
    })
  ->Map.String.fromArray;
};

let find_root = map => {
  let entry_key = Map.String.minKey(map);

  let rec find = next_key => {
    let value = Map.String.findFirstBy(map, (k, _) => k == next_key);
    switch (value) {
    | None => next_key
    | Some(p) =>
      let (_, parent) = p;
      find(parent);
    };
  };

  switch (entry_key) {
  | None => "not found"
  | Some(k) => find(k)
  };
};

let root = input->parse->find_root;

let map_of_programs = input->parse;
let map_of_weight = input->parse_weight;

let get_children = current =>
  Array.map(current, child => {
    Map.String.keep(map_of_programs, (_, v) => v == child)
    ->Map.String.toArray
    ->Array.map(child => {
        let (name_of_child, _) = child;
        name_of_child;
      })
  })
  ->Array.reduce([||], (acc, item) => Array.concat(acc, item));

let sum_of_weight = name => {
  let rec sum_of_weight' = cur_children => {
    let children = get_children(cur_children);

    let weight_of_children =
      children->Array.reduce(
        0,
        (sum, child) => {
          let weight = Map.String.getWithDefault(map_of_weight, child, 0);
          sum + weight;
        },
      );

    Array.length(children) > 0
      ? weight_of_children + sum_of_weight'(Array.copy(children)) : 0;
  };

  sum_of_weight'([|name|])
  + Map.String.getWithDefault(map_of_weight, name, 0);
};

[|root|]->get_children->Js.log;
[|root|]
->get_children
->Array.map(child => Map.String.getWithDefault(map_of_weight, child, 0))
->Js.log;
[|root|]->get_children->Array.map(sum_of_weight)->Js.log;

[|"bntzksk"|]->get_children->Js.log;
[|"bntzksk"|]
->get_children
->Array.map(child => Map.String.getWithDefault(map_of_weight, child, 0))
->Js.log;
[|"bntzksk"|]->get_children->Array.map(sum_of_weight)->Js.log;

let part1 = root->Js.log;
let part2 =
  (Map.String.getWithDefault(map_of_weight, "vmttcwe", 0) - 8)->Js.log;
