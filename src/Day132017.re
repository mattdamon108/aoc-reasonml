/**
 * Advent of Code 2017 Day13
 */
open Belt;

/**
 * how many steps need to come back to the top
 *
 * 0 -> 0
 * 1 -> 0 = (1 - 1) * 2 = 0
 * 2 -> 2 = (2 - 1) * 2 = 2
 * 3 -> 4 = (3 - 1) * 2 = 4
 * 4 -> 6 = (4 - 1) * 2 = 6
 * 5 -> 8 = (5 - 1) * 2 = 8
 * 6 -> 10 = (6 - 1) * 2 = 10
 * 7 -> 12 = (7 - 1) * 2 = 12
 */;

let steps_to_comeback = depth => (depth - 1) * 2;
let get_caught = (layer, depth) => {
  depth == 1 ? true : layer mod depth->steps_to_comeback == 0;
};
let severity = (layer, depth) => layer * depth;

let num_of_caught = input => {
  input->Array.reduce(0, (sum, layer) => {
    switch (layer[0]) {
    | Some(num_of_layer) =>
      switch (layer[1]) {
      | Some(depth) =>
        get_caught(num_of_layer, depth)
          ? sum + severity(num_of_layer, depth) : sum
      | None => sum
      }
    | None => sum
    }
  });
};

// delay to start a trip means layer + delay
let delay_not_to_be_caught = input => {
  let rec simulator = delay => {
    let num_of_caught_after_delay =
      input->Array.reduce(0, (sum, layer) => {
        switch (layer[0]) {
        | Some(num_of_layer) =>
          switch (layer[1]) {
          | Some(depth) =>
            get_caught(num_of_layer + delay, depth) ? sum + 1 : sum
          | None => sum
          }
        | None => sum
        }
      });

    num_of_caught_after_delay === 0 ? delay : simulator(delay + 1);
  };

  simulator(0);
};

let input =
  Node_fs.readFileAsUtf8Sync("input/day13")
  ->Js.String2.split("\n")
  ->Array.map(row => Js.String2.split(row, ": ")->Array.map(int_of_string));

let part1 = input->num_of_caught->Js.log;
let part2 = input->delay_not_to_be_caught->Js.log;

/**
 * 새로운 방식을 시도해보려고 했으나 그럴 여지가 별로 없었나보다.. (핑계)
 */;
