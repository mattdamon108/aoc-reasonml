/*
 * Advent of Code 2017 Day02
 */
open Belt;

let input =
  Node_fs.readFileAsUtf8Sync("input/day0201")
  ->Js.String.split("\n", _)
  ->Array.map(Js.String.split("\t", _))
  ->List.fromArray
  ->List.map(a => List.fromArray(a)->List.map(int_of_string));

let listOfHead =
  input->List.map(a =>
    List.reduce(a, 0, (acc, item) => acc > item ? acc : item)
  );

let max = List.reduce(listOfHead, 0, (acc, item) => acc > item ? acc : item);

let listOfTail =
  input->List.map(a =>
    List.reduce(a, max, (acc, item) => acc > item ? item : acc)
  );

let getFlatRemainder = (x, y) =>
  if (x > y) {
    x mod y === 0 ? x / y : 0;
  } else if (y > x) {
    y mod x === 0 ? y / x : 0;
  } else {
    0;
  };

let computeRemainder = xs => {
  List.map(xs, a => {
    List.reduce(xs, 0, (acc, item) => acc + getFlatRemainder(a, item))
  })
  ->List.reduce(0, (+))
  / 2;
};

// let test =
//   [[2, 3, 4, 5], [4, 7, 9, 18]]
//   ->List.map(a => computeRemainder(a, a))
//   ->Js.log;

let part1 =
  List.zip(listOfHead, listOfTail)
  ->List.map(((x, y)) => x - y)
  ->List.reduce(0, (+))
  ->Js.log;

let part2 = input->List.map(computeRemainder)->List.reduce(0, (+))->Js.log;
