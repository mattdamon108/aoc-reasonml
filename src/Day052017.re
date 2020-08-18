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
    let _ = maze[idx] = jump + 1;
    runner(maze, idx + jump, count + 1);
  };
};

let rec runner_hop = (maze, idx, count) => {
  switch (maze[idx]) {
  | None => count
  | Some(jump) =>
    let _ =
      maze[idx] = (
        if (jump >= 3) {
          jump - 1;
        } else {
          jump + 1;
        }
      );
    runner_hop(maze, idx + jump, count + 1);
  };
};

let part1 = input->runner(0, 0)->Js.log;
let part2 = input2->runner_hop(0, 0)->Js.log;

// arr를 새로 let binding 해도 참조 된다.
let arr1 = [|1, 2, 3|];
let arr2 = arr1;
arr2[0] = 5;
arr1->Js.log; // [|5, 2, 3|]
arr2->Js.log; // [|5, 2, 3|]

/*
 * 1. Array의 mutable 주의
 * 2. Array의 경우 let binding을 했는데도, 왜 참조되어 mutate되는 거지?
 * 3. if-else 는 expression
 * 4. arr[idx] = 1 자체도 bool을 반환하는 expression
 */
