open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/d16test")
  ->Js.String2.trim
  ->Js.String2.split("\n\n");

let inputTest2 =
  Node_fs.readFileAsUtf8Sync("input/2020/d16test2")
  ->Js.String2.trim
  ->Js.String2.split("\n\n")
  ->Array.map(i => {i->Js.String2.split("\n")});

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/d16")
  ->Js.String2.trim
  ->Js.String2.split("\n\n");

let input2 =
  Node_fs.readFileAsUtf8Sync("input/2020/d16")
  ->Js.String2.trim
  ->Js.String2.split("\n\n")
  ->Array.map(i => {i->Js.String2.split("\n")});

let rec parseTest = (str, re, acc) => {
  let result = Js.Re.exec_(re, str);
  switch (result) {
  | Some(r) =>
    let from =
      r
      ->Js.Re.captures
      ->Array.getExn(1)
      ->Js.Nullable.toOption
      ->Option.getExn
      ->int_of_string;
    let to_ =
      r
      ->Js.Re.captures
      ->Array.getExn(2)
      ->Js.Nullable.toOption
      ->Option.getExn
      ->int_of_string;
    parseTest(str, re, acc->Array.concat(Array.range(from, to_)));
  | None => acc
  };
};

let rec parseNumbers = (str, re, acc) => {
  let result = Js.Re.exec_(re, str);
  switch (result) {
  | Some(r) =>
    let num =
      r
      ->Js.Re.captures
      ->Array.getExn(0)
      ->Js.Nullable.toOption
      ->Option.getExn
      ->int_of_string;
    parseNumbers(str, re, acc->Array.concat([|num|]));
  | None => acc
  };
};

let parse = lines => {
  lines->Array.map(line => {
    let reTest = [%re "/(\d+)\-(\d+)/g"];
    let re = [%re "/(\d+)\-(\d+)/g"];
    let reNums = [%re "/\d+/g"];
    let isTest = Js.Re.test_(reTest, line);
    isTest ? line->parseTest(re, [||]) : line->parseNumbers(reNums, [||]);
  });
};

let checkValid = nums => {
  let test = nums->Array.getExn(0)->Set.Int.fromArray;
  let numbers = nums->Array.getExn(2);
  numbers->Array.reduce(0, (acc, n) => {
    test->Set.Int.has(n) ? acc : acc + n
  });
};

let process = lines => {
  let tests = lines->Array.getExn(0);
  let myTicket = lines->Array.getExn(1);
  let tickets = lines->Array.getExn(2);

  let testMap =
    tests->Array.reduce(
      Map.String.empty,
      (acc, test) => {
        let name = test->Js.String2.split(":")->Array.getExn(0);
        let re = [%re "/(\d+)\-(\d+)/g"];
        let values = parseTest(test, re, [||])->Set.Int.fromArray;
        acc->Map.String.set(name, values);
      },
    );

  let reMyTicket = [%re "/\d+/g"];
  let myTicketArr =
    myTicket->Array.getExn(1)->parseNumbers(reMyTicket, [||]);

  let ticketMatrix =
    tickets
    ->Array.sliceToEnd(1)
    ->Array.keep(t => {
        let splitted =
          t->Js.String2.split(",")->Array.map(i => i->int_of_string);
        testMap->Map.String.reduce(false, (acc, k, set) => {
          acc ? acc : splitted->Array.every(spl => {set->Set.Int.has(spl)})
        });
      })
    ->Array.reduceWithIndex(Map.Int.empty, (acc, ticket, idxY) => {
        ticket
        ->Js.String2.split(",")
        ->Array.reduceWithIndex(
            acc,
            (ac, t, idxX) => {
              let prev = ac->Map.Int.get(idxX);
              switch (prev) {
              | Some(p) =>
                ac->Map.Int.set(idxX, p->Array.concat([|t->int_of_string|]))
              | None => ac->Map.Int.set(idxX, [|t->int_of_string|])
              };
            },
          )
      });

  (
    myTicketArr,
    testMap
    ->Map.String.map(set => {
        ticketMatrix
        ->Map.Int.keep((k, v) => {
            v->Array.length
            === v->Array.keep(i => {set->Set.Int.has(i)})->Array.length
          })
        ->Map.Int.keysToArray
      })
    ->Map.String.toArray,
  );
};

let checkMyTicket = ((myTicket, testArr)) => {
  let rec sort = (ta, acc) => {
    let field =
      ta->Array.keep(t => {
        let (name, values) = t;
        values->Array.length === 1;
      });
    let length = field->Array.length;
    length === 0
      ? acc
      : {
        let (name, cols) = field->Array.getExn(0);
        let col = cols->Array.getExn(0);
        let newAcc = acc->Array.concat([|(name, col)|]);
        let newTa =
          ta
          ->Array.map(t => {
              let (n, values) = t;
              let newValues = values->Array.keep(i => {i !== col});
              (n, newValues);
            })
          ->Array.keep(t => {
              let (n, _) = t;
              n !== name;
            });

        sort(newTa, newAcc);
      };
  };

  sort(testArr, [||])
  ->Array.reduce(
      1.0,
      (acc, t) => {
        let (name, col) = t;
        if (name->Js.String2.includes("departure")) {
          acc *. myTicket->Array.getExn(col)->float_of_int;
        } else {
          acc;
        };
      },
    );
};

// part1
input->parse->checkValid->Js.log;

// part2
input2->process->checkMyTicket->Js.log;
