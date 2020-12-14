open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day13test")
  ->Js.String2.trim
  ->Js.String2.split("\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day13")
  ->Js.String2.trim
  ->Js.String2.split("\n");

let getStartTime = input => {
  input->Array.getExn(0);
};

let getTimetable = input => {
  input->Array.getExn(1)->Js.String2.split(",");
};

let waitTime = input => {
  let startTime = input->getStartTime->int_of_string;
  let timetable =
    input->getTimetable->Array.keep(i => i !== "x")->Array.map(int_of_string);
  let (busID, wait) =
    timetable
    ->Array.map(t =>
        if (startTime mod t === 0) {
          (t, 0);
        } else {
          (t, (startTime / t + 1) * t - startTime);
        }
      )
    ->Array.reduce(
        (0, max_int),
        (acc, item) => {
          let (_, accWait) = acc;
          let (_, wait) = item;
          if (accWait > wait) {
            item;
          } else {
            acc;
          };
        },
      );

  busID * wait;
};

let timestamp = input => {
  let timetable =
    input
    ->getTimetable
    ->Array.mapWithIndex((idx, busId) => {(idx, busId)})
    ->Array.keep(i => {
        let (_, t) = i;
        t !== "x";
      })
    ->Array.map(i => {
        let (offset, busId) = i;
        (offset->float_of_int, busId->float_of_string);
      });

  timetable->Array.reduce(
    (0.0, 1.0),
    (acc, (offset, busId)) => {
      // 어긋난 최소공배수?
      let (lcm, m) = acc;
      let rec compute = n => {
        let num = lcm +. m *. n;
        let result =
          if (offset === 0.0) {
            mod_float(num, busId) === 0.0;
          } else {
            mod_float(num, busId) === busId -. mod_float(offset, busId);
          };

        if (result) {
          (num, m *. busId);
        } else {
          compute(n +. 1.0);
        };
      };

      compute(1.0);
    },
  );
};

// part1
input->waitTime->Js.log; // 3789

// part2
input->timestamp->Js.log;
