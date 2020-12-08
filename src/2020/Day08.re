open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day08test")
  ->Js.String2.trim
  ->Js.String2.split("\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day08")
  ->Js.String2.trim
  ->Js.String2.split("\n");

module Interpreter = {
  type t;

  let parse = input => {
    input->Array.map(i => {
      let splitted = i->Js.String2.split(" ");
      let value = splitted->Array.getExn(1)->int_of_string;
      (splitted->Array.getExn(0), value);
    });
  };

  let rec interpret = (inst, cur, acc, history) =>
    if (history->Array.some(h => h === cur)) {
      Error(acc);
    } else if (cur === inst->Array.length - 1) {
      let (command, value) = inst->Array.getExn(cur);
      switch (command) {
      | "nop" => Ok(acc)
      | "acc" => Ok(acc + value)
      | "jmp" => Ok(acc)
      | _ => Ok(acc)
      };
    } else {
      let (command, value) = inst->Array.getExn(cur);
      let newHistory = history->Array.concat([|cur|]);
      switch (command) {
      | "nop" => interpret(inst, cur + 1, acc, newHistory)
      | "acc" => interpret(inst, cur + 1, acc + value, newHistory)
      | "jmp" => interpret(inst, cur + value, acc, newHistory)
      | _ => interpret(inst, cur, acc, history)
      };
    };

  let run = instructions => {
    switch (instructions->interpret(0, 0, [||])) {
    | Error(acc) => acc
    | _ => (-1) // something wrong here;
    };
  };

  let patch = brokenInstructions => {
    let finder = (instructions, start) => {
      let rec seek = pos => {
        let (command, value) = instructions->Array.getExn(pos);
        command === "nop" || command === "jmp"
          ? (command, value, pos) : seek(pos + 1);
      };

      seek(start);
    };

    let rec run = (instructions, brokenPos) => {
      switch (instructions->interpret(0, 0, [||])) {
      | Ok(ac) => ac
      | Error(_) =>
        let (command, value, newBrokenPos) = finder(instructions, brokenPos);
        let newCommand =
          switch (command) {
          | "nop" => "jmp"
          | "jmp" => "nop"
          | _ => command
          };
        let newInstructions = Array.copy(brokenInstructions);
        newInstructions->Array.setUnsafe(newBrokenPos, (newCommand, value));
        run(newInstructions, newBrokenPos + 1);
      };
    };

    run(brokenInstructions, 0);
  };
};

// part1
input->Interpreter.parse->Interpreter.run->Js.log;

// part2
input->Interpreter.parse->Interpreter.patch->Js.log;
