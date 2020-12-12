open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day12test")
  ->Js.String2.trim
  ->Js.String2.split("\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day12")
  ->Js.String2.trim
  ->Js.String2.split("\n");

type command =
  | North
  | South
  | East
  | West
  | Left
  | Right
  | Forward;

let parseInstruction = code => {
  let re = [%re "/^(\w)(\d+)/g"];
  let%Opt result = Js.Re.exec_(re, code);
  let%Opt command =
    result->Js.Re.captures->Array.getExn(1)->Js.Nullable.toOption;
  let command' =
    switch (command) {
    | "N" => North
    | "S" => South
    | "E" => East
    | "W" => West
    | "L" => Left
    | "R" => Right
    | "F" => Forward
    | _ => Forward
    };
  let%Opt value =
    result->Js.Re.captures->Array.getExn(2)->Js.Nullable.toOption;
  let value' = value->int_of_string;
  Some((command', value'));
};

module Direction = {
  type t =
    | N
    | S
    | E
    | W;

  let countClockwise = [|N, W, S, E|];
  let clockwise = [|N, E, S, W|];

  let toValue = dir => {
    switch (dir) {
    | N => (0, (-1))
    | S => (0, 1)
    | E => (1, 0)
    | W => ((-1), 0)
    };
  };

  let changeDirection = (dir, (command, degree)) => {
    switch (command) {
    | Left =>
      let idx =
        countClockwise->Array.getIndexBy(d => d === dir)->Option.getExn;
      countClockwise->Array.getExn((idx + degree / 90) mod 4);
    | Right =>
      let idx = clockwise->Array.getIndexBy(d => d === dir)->Option.getExn;
      clockwise->Array.getExn((idx + degree / 90) mod 4);
    | _ => dir
    };
  };
};

module WayPoint = {
  type t = (int, int);

  let move = (wayPoint, code) => {
    let (command, value) = code;
    let (x, y) = wayPoint;

    switch (command) {
    | North => (x, y - value)
    | South => (x, y + value)
    | East => (x + value, y)
    | West => (x - value, y)
    | Right =>
      switch (value) {
      | 90 => (- y, x)
      | 180 => (- x, - y)
      | 270 => (y, - x)
      | 360 => wayPoint
      | _ => wayPoint
      }
    | Left =>
      switch (value) {
      | 90 => (y, - x)
      | 180 => (- x, - y)
      | 270 => (- y, x)
      | 360 => wayPoint
      | _ => wayPoint
      }
    | _ => wayPoint
    };
  };
};

module Ship = {
  type t = (int, int);

  let move = (cur, dir, code) => {
    let (x, y) = cur;
    let (command, value) = code;

    let (dx, dy) = Direction.toValue(dir);

    switch (command) {
    | Forward => ((x + value * dx, y + value * dy), dir)
    | North => ((x, y - value), dir)
    | South => ((x, y + value), dir)
    | East => ((x + value, y), dir)
    | West => ((x - value, y), dir)
    | _ =>
      let newDir = Direction.changeDirection(dir, code);
      (cur, newDir);
    };
  };

  let move2 = ((wayPoint, cur), code) => {
    let (wx, wy) = wayPoint;
    let (command, value) = code;
    let (x, y) = cur;

    let newWayPoint = WayPoint.move(wayPoint, code);
    switch (command) {
    | Forward => (wayPoint, (x + wx * value, y + wy * value))
    | _ => (newWayPoint, (x, y))
    };
  };

  let sail = codes => {
    codes
    ->Array.map(parseInstruction)
    ->Array.reduce(
        ((0, 0), Direction.E),
        (cur, code) => {
          let (pos, dir) = cur;
          switch (code) {
          | Some(c) => move(pos, dir, c)
          | None => cur
          };
        },
      );
  };

  let sail2 = codes => {
    codes
    ->Array.map(parseInstruction)
    ->Array.reduce(((10, (-1)), (0, 0)), (cur, code) => {
        switch (code) {
        | Some(c) => move2(cur, c)
        | None => cur
        }
      });
  };
};

// part1
input->Ship.sail->Js.log /* abs(-1056 + -121) => 1177*/;

// part2
input->Ship.sail2->Js.log; // 31488, 15042
