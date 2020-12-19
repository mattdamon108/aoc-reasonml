open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/d18test")
  ->Js.String2.trim
  ->Js.String2.split("\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/d18")
  ->Js.String2.trim
  ->Js.String2.split("\n");

type char =
  | Add
  | Multiply
  | ParenOpen
  | ParenClose
  | Space
  | Num(float);

exception UndefinedOperator;

let parse = line => {
  line
  ->Js.String2.split("")
  ->Array.map(c => {
      switch (c) {
      | "+" => Add // 0
      | "*" => Multiply // 1
      | " " => Space // 4
      | "(" => ParenOpen // 2
      | ")" => ParenClose // 3
      | _ => Num(c->float_of_string) // 5
      }
    })
  ->Array.keep(c => c !== Space);
};

let addParenForward = arr => {
  let rec compute = (a, paren, idx) => {
    let length = a->Array.length;
    if (idx === length - 1) {
      a->Array.concat([|ParenOpen|])->Array.reverse;
    } else {
      let char = a->Array.getExn(idx);
      let front = a->Array.slice(~offset=0, ~len=idx);
      let back = a->Array.sliceToEnd(idx + 1);
      switch (char) {
      | Multiply =>
        if (paren === 0) {
          Array.concatMany([|front, [|ParenOpen, Multiply|], back|])
          ->Array.reverse;
        } else {
          compute(a, paren, idx + 1);
        }
      | ParenClose => compute(a, paren + 1, idx + 1)
      | ParenOpen =>
        if (paren === 0) {
          Array.concatMany([|front, [|ParenOpen, ParenOpen|], back|])
          ->Array.reverse;
        } else {
          compute(a, paren - 1, idx + 1);
        }
      | _ => compute(a, paren, idx + 1)
      };
    };
  };

  compute(arr->Array.reverse, 0, 0);
};

let addParenBackward = arr => {
  let rec compute = (a, paren, idx) => {
    let length = a->Array.length;
    if (idx === length - 1) {
      a->Array.concat([|ParenClose|]);
    } else {
      let char = a->Array.getExn(idx);
      let front = a->Array.slice(~offset=0, ~len=idx);
      let back = a->Array.sliceToEnd(idx + 1);
      switch (char) {
      | Multiply =>
        if (paren === 0) {
          Array.concatMany([|front, [|ParenClose, Multiply|], back|]);
        } else {
          compute(a, paren, idx + 1);
        }
      | ParenOpen => compute(a, paren + 1, idx + 1)
      | ParenClose =>
        if (paren === 0) {
          Array.concatMany([|front, [|ParenClose, ParenClose|], back|]);
        } else {
          compute(a, paren - 1, idx + 1);
        }
      | _ => compute(a, paren, idx + 1)
      };
    };
  };

  compute(arr, 0, 0);
};

let addParen = line => {
  let rec runner = (l, idx) => {
    let length = l->Array.length;

    if (idx === length - 1) {
      l;
    } else {
      let char = l->Array.getExn(idx);
      switch (char) {
      | Add =>
        let front = l->Array.slice(~offset=0, ~len=idx);
        let back = l->Array.sliceToEnd(idx);
        let newFront = front->addParenForward;
        let newBack = back->addParenBackward;
        runner(Array.concatMany([|newFront, newBack|]), idx + 2);
      | _ => runner(l, idx + 1)
      };
    };
  };

  runner(line, 0);
};

let calculate = (n, m, op) => {
  switch (op) {
  | Add => n +. m
  | Multiply => n *. m
  | _ => raise(UndefinedOperator)
  };
};

let rec compute = line => {
  let rec runner = (l, acc, op) =>
    if (l->Array.length === 0) {
      (acc, [||]);
    } else {
      let first = l->Array.getExn(0);
      let rest = l->Array.sliceToEnd(1);
      switch (first) {
      | Num(i) => runner(rest, calculate(acc, i, op), op)
      | Add => runner(rest, acc, Add)
      | Multiply => runner(rest, acc, Multiply)
      | ParenOpen =>
        let (ac, rrest) = compute(rest);
        runner(rrest, calculate(acc, ac, op), op);
      | ParenClose => (acc, rest)
      | _ => (acc, rest)
      };
    };

  runner(line, 0.0, Add);
};

// part1
input
->Array.map(parse)
->Array.map(compute)
->Array.reduce(
    0.0,
    (acc, i) => {
      let (value, _) = i;
      acc +. value;
    },
  )
->Js.log;

// part2
input
->Array.map(parse)
->Array.map(addParen)
->Array.map(compute)
->Array.reduce(
    0.0,
    (acc, i) => {
      let (value, _) = i;
      acc +. value;
    },
  )
->Js.log;
