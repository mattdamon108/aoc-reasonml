open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/d19test")
  ->Js.String2.trim
  ->Js.String2.split("\n\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/d19")
  ->Js.String2.trim
  ->Js.String2.split("\n\n");

exception UndefinedRuleZero;
exception UndefinedRule;
exception UndefinedRuleMsg;
exception UndefinedPath;

let parse = line => {
  let reRuleZero = [%re "/^0: (\d+) (\d+) (\d+)?/"];
  let reRuleOr = [%re "/\|/"];
  let reRule = [%re
    "/^(\d+): (\d+)\s?(\d+)?\s?\|?\s?(\d+)?\s?(\d+)?\s?(\d+)?/"
  ];
  let reRuleMsg = [%re "/^(\d+): \"(\w+)\"/"];

  let isRuleZero = line |> Js.Re.test_(reRuleZero);
  let isRuleMsg = line |> Js.Re.test_(reRuleMsg);
  let isRuleOr = line |> Js.Re.test_(reRuleOr);
  if (isRuleZero) {
    let result = line |> Js.Re.exec_(reRuleZero);
    switch (result) {
    | Some(r) =>
      let first = r->Js.Re.captures->Array.getExn(1)->Js.Nullable.toOption;
      let second = r->Js.Re.captures->Array.getExn(2)->Js.Nullable.toOption;
      let third = r->Js.Re.captures->Array.getExn(3)->Js.Nullable.toOption;
      (0, first, second, third, None, None, 0);
    | None => raise(UndefinedRuleZero)
    };
  } else if (isRuleMsg) {
    let result = line |> Js.Re.exec_(reRuleMsg);
    switch (result) {
    | Some(r) =>
      let ruleNum =
        r
        ->Js.Re.captures
        ->Array.getExn(1)
        ->Js.Nullable.toOption
        ->Option.getExn
        ->int_of_string;
      let msg = r->Js.Re.captures->Array.getExn(2)->Js.Nullable.toOption;
      (ruleNum, msg, None, None, None, None, 0);
    | None => raise(UndefinedRuleMsg)
    };
  } else {
    let result = line |> Js.Re.exec_(reRule);
    if (isRuleOr) {
      let posOr =
        line
        ->Js.String2.split(": ")
        ->Array.getExn(1)
        ->Js.String2.split(" | ")
        ->Array.getExn(0)
        ->Js.String2.split(" ")
        ->Array.length;
      switch (result) {
      | Some(r) =>
        let ruleNum =
          r
          ->Js.Re.captures
          ->Array.getExn(1)
          ->Js.Nullable.toOption
          ->Option.getExn
          ->int_of_string;
        let first = r->Js.Re.captures->Array.getExn(2)->Js.Nullable.toOption;
        let second = r->Js.Re.captures->Array.getExn(3)->Js.Nullable.toOption;
        let third = r->Js.Re.captures->Array.getExn(4)->Js.Nullable.toOption;
        let fourth = r->Js.Re.captures->Array.getExn(5)->Js.Nullable.toOption;
        let fifth = r->Js.Re.captures->Array.getExn(6)->Js.Nullable.toOption;
        (ruleNum, first, second, third, fourth, fifth, posOr);
      | None => raise(UndefinedRule)
      };
    } else {
      switch (result) {
      | Some(r) =>
        let ruleNum =
          r
          ->Js.Re.captures
          ->Array.getExn(1)
          ->Js.Nullable.toOption
          ->Option.getExn
          ->int_of_string;
        let first = r->Js.Re.captures->Array.getExn(2)->Js.Nullable.toOption;
        let second = r->Js.Re.captures->Array.getExn(3)->Js.Nullable.toOption;
        let third = r->Js.Re.captures->Array.getExn(4)->Js.Nullable.toOption;
        let fourth = r->Js.Re.captures->Array.getExn(5)->Js.Nullable.toOption;
        let fifth = r->Js.Re.captures->Array.getExn(6)->Js.Nullable.toOption;
        (ruleNum, first, second, third, fourth, fifth, 0);
      | None => raise(UndefinedRule)
      };
    };
  };
};

let rules =
  input
  ->Array.getExn(0)
  ->Js.String2.split("\n")
  ->Array.map(line => line->parse);

let messages = input->Array.getExn(1)->Js.String2.split("\n");

type rule =
  | Top(int, int, option(int))
  | RuleOr(
      option(int),
      option(int),
      option(int),
      option(int),
      option(int),
      int,
    )
  | RuleAnd(
      option(int),
      option(int),
      option(int),
      option(int),
      option(int),
      int,
    )
  | Msg(string);

let mapOfRules =
  rules->Array.reduce(
    Map.Int.empty,
    (acc, line) => {
      let (ruleNum, a, b, c, d, e, posOr) = line;
      if (ruleNum === 0) {
        let first = a->Option.getExn->int_of_string;
        let second = b->Option.getExn->int_of_string;
        let third = c->Option.map(t => t->int_of_string);
        switch (c) {
        | Some(_) => acc->Map.Int.set(0, Top(first, second, third))
        | None => acc->Map.Int.set(0, Top(first, second, None))
        };
      } else {
        let first = a->Option.getExn->Int.fromString;
        switch (first) {
        // Rule
        | Some(_) =>
          if (posOr > 0) {
            acc->Map.Int.set(
              ruleNum,
              RuleOr(
                a->Option.map(i => i->int_of_string),
                b->Option.map(i => i->int_of_string),
                c->Option.map(i => i->int_of_string),
                d->Option.map(i => i->int_of_string),
                e->Option.map(i => i->int_of_string),
                posOr,
              ),
            );
          } else {
            acc->Map.Int.set(
              ruleNum,
              RuleAnd(
                a->Option.map(i => i->int_of_string),
                b->Option.map(i => i->int_of_string),
                c->Option.map(i => i->int_of_string),
                d->Option.map(i => i->int_of_string),
                e->Option.map(i => i->int_of_string),
                0,
              ),
            );
          }
        // Msg
        | None => acc->Map.Int.set(ruleNum, Msg(a->Option.getExn))
        };
      };
    },
  );

let mapRule = (count, f, r) => {
  switch (r) {
  | Some(r') =>
    if ((r' === 8 || r' === 11) && count > 14) {
      "";
    } else {
      f(count+ 1, r');
    }
  | None => ""
  };
};

let generateRegex = map => {
  let rec generate = (count, ruleNum) => {
    let rule = map->Map.Int.get(ruleNum);
    let mapRuleGen = mapRule(count, generate);
    switch (rule) {
    | Some(Msg(c)) => c
    | Some(RuleAnd(a, b, c, d, e, _)) =>
      Js.String2.concatMany(
        mapRuleGen(a),
        [|mapRuleGen(b), mapRuleGen(c), mapRuleGen(d), mapRuleGen(e)|],
      )
    | Some(RuleOr(a, b, c, d, e, posOr)) =>
      switch (posOr) {
      | 1 =>
        Js.String2.concatMany(
          "(",
          [|
            mapRuleGen(a),
            "|",
            mapRuleGen(b),
            mapRuleGen(c),
            mapRuleGen(d),
            mapRuleGen(e),
            ")",
          |],
        )
      | _ =>
        Js.String2.concatMany(
          "(",
          [|
            mapRuleGen(a),
            mapRuleGen(b),
            "|",
            mapRuleGen(c),
            mapRuleGen(d),
            mapRuleGen(e),
            ")",
          |],
        )
      }
    | Some(Top(a, b, c)) =>
      Js.String2.concatMany(
        generate(count, a),
        [|
          generate(count, b),
          switch (c) {
          | Some(c') => c'->generate(count)
          | None => ""
          },
        |],
      )
    | _ => raise(UndefinedRule)
    };
  };

  Js.String2.concatMany("^", [|generate(0, 0), "$"|]);
};

let reMessage = Js.Re.fromString(mapOfRules->generateRegex);

messages
->Array.map(m => {m |> Js.Re.test_(reMessage)})
->Array.reduce(0, (acc, m) => {m ? acc + 1 : acc})
->Js.log;
