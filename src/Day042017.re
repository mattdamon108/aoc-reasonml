/*
 * Advent of Code 2017 Day04
 */

open Belt;

let input =
  Node_fs.readFileAsUtf8Sync("input/day04")
  ->Js.String.split("\n", _)
  ->Array.map(row => Js.String.trim(row)->Js.String.split(" ", _));

let is_valid = arr => {
  let unique_length =
    Set.String.fromArray(arr)->Set.String.toArray(_)->Array.length(_);
  Array.length(arr) == unique_length;
};

let part1 =
  input
  ->Array.map(row => is_valid(row))
  ->Array.reduce(0, (acc, item) => item ? acc + 1 : acc)
  ->Js.log;

let is_valid_anagram = arr => {
  let converted_to_set =
    Array.map(arr, item =>
      Js.String.split("", item)->Set.String.fromArray(_)
    );
  Array.map(converted_to_set, pw
    // start from -1 because of self-matching case
    =>
      Array.reduce(converted_to_set, -1, (acc, item) =>
        Set.String.eq(pw, item) ? acc + 1 : acc
      )
    )
  ->Array.reduce(0, (+))
  == 0;
};

let part2 =
  input
  ->Array.map(row => is_valid_anagram(row) ? 1 : 0)
  ->Array.reduce(0, (+))
  ->Js.log;
