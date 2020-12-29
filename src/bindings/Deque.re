type deque;
[@bs.module] [@bs.new]
external make: array('a) => deque = "double-ended-queue";
[@bs.send] external push:(deque, 'a) => unit = "push";
[@bs.send] external unshift:(deque, 'a) => unit = "unshift";
[@bs.send] external pop: deque => 'a = "pop";
[@bs.send] external shift: deque => 'a = "shift";

let dq = make([|1, 2, 3, 4|]);
dq->Js.log;
dq->shift->Js.log;
dq->pop->Js.log;
dq->push(1)
dq->Js.log;
