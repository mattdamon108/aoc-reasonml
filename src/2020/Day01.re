open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day01test")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n")
  ->Array.map(int_of_string);

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day01")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n")
  ->Array.map(int_of_string);

let rec compute2 = (xs, num) => {
  let shifted = Belt.Array.shuffle(xs);
  let zipped = Belt.Array.zip(xs, shifted);
  let (a, b) =
    zipped->Belt.Array.reduce((0, 0), (acc, (a, b)) =>
      if (a + b == num) {
        (a, b);
      } else {
        acc;
      }
    );

  if (a != 0 && b != 0) {
    (a, b);
  } else {
    compute2(shifted, num);
  };
};

let rec compute3 = (xs, num) => {
  let shifted1 = Belt.Array.shuffle(xs);
  let shifted2 = Belt.Array.shuffle(xs);
  let zipped = Belt.Array.zip(xs, shifted1)->Belt.Array.zip(shifted2);
  let (a, b, c) =
    zipped->Belt.Array.reduce((0, 0, 0), (acc, ((a, b), c)) =>
      if (a + b + c == num) {
        (a, b, c);
      } else {
        acc;
      }
    );

  if (a != 0 && b != 0 && c != 0) {
    (a, b, c);
  } else {
    compute3(shifted1, num);
  };
};

let (a, b) = compute2(input, 2020);
let part1 = a * b;
part1->Js.log;

let (a, b, c) = compute3(input, 2020);
let part2 = a * b * c;
part2->Js.log;
