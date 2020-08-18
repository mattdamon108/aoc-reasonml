/*
 * Advent of Code 2017 Day05
 */

open Belt;

let input =
  Node_fs.readFileAsUtf8Sync("input/day05")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n")
  ->Array.map(int_of_string);

let input2 = Array.copy(input);

let rec runner = (maze, idx, count) => {
  switch (maze[idx]) {
  | None => count
  | Some(jump) =>
    let new_maze = maze;
    let _ = new_maze[idx] = jump + 1;
    runner(new_maze, idx + jump, count + 1);
  };
};

let rec runner_hop = (maze, idx, count) => {
  switch (maze[idx]) {
  | None => count
  | Some(jump) =>
    let new_maze = maze;
    let _ =
      new_maze[idx] = (
        if (jump >= 3) {
          jump - 1;
        } else {
          jump + 1;
        }
      );
    runner_hop(new_maze, idx + jump, count + 1);
  };
};

let part1 = input->runner(0, 0)->Js.log;
let part2 = input2->runner_hop(0, 0)->Js.log;

/*
 * 1. Array의 mutable 주의
 * 2. if-else 는 expression
 * 3. arr[idx] = 1 자체도 bool을 반환하는 expression
 */
