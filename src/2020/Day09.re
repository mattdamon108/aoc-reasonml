open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day09test")
  ->Js.String2.trim
  ->Js.String2.split("\n")
  ->Array.map(i => i->float_of_string);

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day09")
  ->Js.String2.trim
  ->Js.String2.split("\n")
  ->Array.map(i => i->float_of_string);

let checkValid = (nums, start, len) => {
  let target = nums->Array.getExn(start + len);
  let preamble = nums->Array.slice(~offset=start, ~len);
  preamble->Array.some(a => {
    preamble->Array.some(b =>
      if (a === b) {
        false;
      } else {
        a +. b === target ? true : false;
      }
    )
  });
};

let rec findInvalid = (nums, start, len) => {
  let count = nums->Array.length;
  let isValid = checkValid(nums, start, len);
  if (!isValid || start + len > count) {
    nums->Array.getExn(start + len);
  } else {
    findInvalid(nums, start + 1, len);
  };
};

let sum = (nums, start, len) => {
  Array.range(start, start + len - 1)
  ->Array.map(i => {nums->Array.getExn(i)})
  ->Array.reduce(0.0, (+.));
};

let invalidNum = input->findInvalid(0, 25);
let invalidNumTest = inputTest->findInvalid(0, 5);

let findNumbers = (nums, target) => {
  let largest = nums->Array.reduce(0.0, max);
  let rec compute = (n, s, l) => {
    let count = n->Array.length;
    if (!(s + l > count)) {
      let found = sum(n, s, l) === target;
      if (found) {
        let weaknessNumbers =
          Array.range(s, s + l - 1)
          ->Array.map(idx => nums->Array.getExn(idx));
        let minNum = weaknessNumbers->Array.reduce(largest, min);
        let maxNum = weaknessNumbers->Array.reduce(0.0, max);
        minNum +. maxNum;
      } else {
        compute(n, s + 1, l);
      };
    } else {
      compute(n, 0, l + 1);
    };
  };

  compute(nums, 0, 2);
};

// part1
invalidNum->Js.log;

// part2
input->findNumbers(invalidNum)->Js.log;
