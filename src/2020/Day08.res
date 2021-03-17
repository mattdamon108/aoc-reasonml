open Belt

let inputTest = Node_fs.readFileAsUtf8Sync("input/2020/day08test")->Js.String2.trim

let input = Node_fs.readFileAsUtf8Sync("input/2020/day08")->Js.String2.trim

module type Interpreter = {
  type instruction
  type state
  let parse: string => array<instruction>
  let patch: instruction => instruction
  let run: array<instruction> => int
  let runWithPatch: (array<instruction>, ~patchFn: instruction => instruction) => int
}

module Interpreter: Interpreter = {
  type instruction = NOP(int) | ACC(int) | JMP(int)
  type cursor = int
  type state = {
    cursor: cursor,
    acc: int,
    log: list<cursor>,
  }

  let parse = data =>
    data
    ->Js.String2.split("\n")
    ->Array.keepMap(line => {
      let splitted = line->Js.String2.trim->Js.String2.split(" ")
      let command = splitted[0]
      let value = splitted[1]->Option.flatMap(v => v->Int.fromString)
      switch (command, value) {
      | (Some(c), Some(v)) =>
        switch c {
        | "nop" => Some(NOP(v))
        | "acc" => Some(ACC(v))
        | "jmp" => Some(JMP(v))
        | _ => None
        }
      | (_, _) => None
      }
    })

  let rec interpret = (instructions: array<instruction>, state) =>
    if state.log->List.some(h => h === state.cursor) {
      // 이미 실행한 instruction이 있을 때
      Error(state)
    } else if state.cursor === instructions->Array.length - 1 {
      // instruction을 끝까지 다 수행했을 때
      switch instructions->Array.getExn(state.cursor) {
      | NOP(_) => Ok(state)
      | ACC(v) => Ok({...state, acc: state.acc + v})
      | JMP(_) => Ok(state)
      }
    } else {
      // 계속 진행하는 경우
      // let (command, value) = inst->Array.getExn(cur)
      let newLog = state.log->List.add(state.cursor)
      switch instructions->Array.getExn(state.cursor) {
      | NOP(_) => interpret(instructions, {...state, cursor: state.cursor + 1, log: newLog})
      | ACC(v) =>
        interpret(instructions, {cursor: state.cursor + 1, acc: state.acc + v, log: newLog})
      | JMP(v) => interpret(instructions, {...state, cursor: state.cursor + v, log: newLog})
      }
    }

  let findBroken = (instructions, start) => {
    let rec seek = pos => {
      switch instructions->Array.getExn(pos) {
      | NOP(v) => (NOP(v), pos)
      | JMP(v) => (JMP(v), pos)
      | ACC(_) => seek(pos + 1)
      }
    }

    seek(start)
  }

  let patch = instruction =>
    switch instruction {
    | NOP(v) => JMP(v)
    | JMP(v) => NOP(v)
    | _ => instruction
    }

  let run = instructions =>
    switch instructions->interpret({cursor: 0, acc: 0, log: list{}}) {
    | Error(state) => state.acc
    | Ok(_) => -1 // Ok가 나오면 이상한 상황!
    }

  let runWithPatch = (instructions, ~patchFn) => {
    let rec patcher = (brokenInstructions, brokenPos) => {
      switch brokenInstructions->interpret({cursor: 0, acc: 0, log: list{}}) {
      | Ok(state) => state.acc
      | Error(_) => {
          let (brokenInstruction, newBrokenPos) = findBroken(brokenInstructions, brokenPos)
          let fixedInstruction = patchFn(brokenInstruction)
          let fixedInstructions = Array.copy(instructions)
          fixedInstructions->Array.setUnsafe(newBrokenPos, fixedInstruction)
          patcher(fixedInstructions, newBrokenPos + 1)
        }
      }
    }

    patcher(instructions, 0)
  }
}

// part1
input->Interpreter.parse->Interpreter.run->Js.log

// part2
input->Interpreter.parse->Interpreter.runWithPatch(~patchFn=Interpreter.patch)->Js.log
