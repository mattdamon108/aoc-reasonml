open Belt

exception ParseError

let inputTest = Node_fs.readFileAsUtf8Sync("input/2020/day04test")->Js.String2.trim(_)

let input = Node_fs.readFileAsUtf8Sync("input/2020/day04")->Js.String2.trim(_)

module type Passport = {
  type unvalidated
  type validated
  type t<'a>
  let empty: t<unvalidated>

  let byr: t<'a> => int
  let iyr: t<'a> => int
  let eyr: t<'a> => int
  let hgt: t<'a> => string
  let hcl: t<'a> => string
  let ecl: t<'a> => string
  let pid: t<'a> => string
  let cid: t<'a> => option<string>

  let extractFnByRe: (Js.Re.t, string) => list<(string, string)>
  let make: (
    string,
    ~extractFn: string => list<(string, string)>,
    ~init: t<unvalidated>,
  ) => t<unvalidated>
  let validate: t<unvalidated> => t<validated>
}

module Passport: Passport = {
  type unvalidated
  type validated
  type passport = {
    byr: int,
    iyr: int,
    eyr: int,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: option<string>,
  }
  /**
   * (??) passport라는 타입을 별도로 만들어서 t<'a>에 바인딩해야
   * validate 함수에 대한 타입이 빌드 통과된다.
   */
  type t<'a> = passport

  let empty: t<unvalidated> = {
    byr: 0,
    iyr: 0,
    eyr: 0,
    hgt: "",
    hcl: "",
    ecl: "",
    pid: "",
    cid: None,
  }

  let byr = (p: t<'a>) => p.byr
  let iyr = (p: t<'a>) => p.iyr
  let eyr = (p: t<'a>) => p.eyr
  let hgt = (p: t<'a>) => p.hgt
  let hcl = (p: t<'a>) => p.hcl
  let ecl = (p: t<'a>) => p.ecl
  let pid = (p: t<'a>) => p.pid
  let cid = (p: t<'a>) => p.cid

  let extractFnByRe = (re, raw) => {
    let rec extract = acc => {
      switch Js.Re.exec_(re, raw) {
      | Some(result) =>
        let captured =
          result->Js.Re.captures->Array.map(Js.Nullable.toOption)->Array.keepMap(x => x)
        switch (captured->Array.get(1), captured->Array.get(2)) {
        | (Some(k), Some(v)) => extract(acc->List.add((k, v)))
        | (_, _) => raise(ParseError)
        }
      | None => acc
      }
    }

    extract(list{})
  }

  let make = (raw, ~extractFn, ~init) => {
    raw
    ->extractFn
    ->List.reduce(init, (passport, field) => {
      let (k, v) = field
      switch k {
      | "byr" => {...passport, byr: v->int_of_string}
      | "iyr" => {...passport, iyr: v->int_of_string}
      | "eyr" => {...passport, eyr: v->int_of_string}
      | "hgt" => {...passport, hgt: v}
      | "hcl" => {...passport, hcl: v}
      | "ecl" => {...passport, ecl: v}
      | "pid" => {...passport, pid: v}
      | "cid" => {...passport, cid: Some(v)}
      | _ => passport
      }
    })
  }

  let validate = a => a
}

let re = %re("/(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)/g")

let checkValidFns1 = list{
  (p: Passport.t<Passport.unvalidated>) => p->Passport.byr > 0 ? Ok(p) : Error("no byr"),
  (p: Passport.t<Passport.unvalidated>) => p->Passport.iyr > 0 ? Ok(p) : Error("no iyr"),
  (p: Passport.t<Passport.unvalidated>) => p->Passport.eyr > 0 ? Ok(p) : Error("no eyr"),
  (p: Passport.t<Passport.unvalidated>) => p->Passport.hgt !== "" ? Ok(p) : Error("no hgt"),
  (p: Passport.t<Passport.unvalidated>) => p->Passport.hcl !== "" ? Ok(p) : Error("no hcl"),
  (p: Passport.t<Passport.unvalidated>) => p->Passport.ecl !== "" ? Ok(p) : Error("no ecl"),
  (p: Passport.t<Passport.unvalidated>) => p->Passport.pid !== "" ? Ok(p) : Error("no pid"),
  // cid는 필요 없음
}

let checkValidFns2 = list{
  // byr
  (p: Passport.t<Passport.unvalidated>) =>
    1920 <= p->Passport.byr && p->Passport.byr <= 2002 ? Ok(p) : Error("no byr"),
  // iyr
  (p: Passport.t<Passport.unvalidated>) =>
    2010 <= p->Passport.iyr && p->Passport.iyr <= 2020 ? Ok(p) : Error("no iyr"),
  // eyr
  (p: Passport.t<Passport.unvalidated>) =>
    2020 <= p->Passport.eyr && p->Passport.eyr <= 2030 ? Ok(p) : Error("no eyr"),
  // hgt
  (p: Passport.t<Passport.unvalidated>) => {
    let re = %re("/^(\d+)(cm|in)$/g")
    switch Js.Re.exec_(re, p->Passport.hgt) {
    | Some(result) =>
      let captured = result->Js.Re.captures->Array.map(Js.Nullable.toOption)->Array.keepMap(x => x)
      switch (captured->Array.get(1)->Option.flatMap(Int.fromString), captured->Array.get(2)) {
      | (Some(h), Some(u)) =>
        switch u {
        | "cm" => 150 <= h && h <= 193 ? Ok(p) : Error("invalid hgt")
        | "in" => 59 <= h && h <= 76 ? Ok(p) : Error("invalid hgt")
        | _ => Error("invalid hgt")
        }
      | (_, _) => Error("invalid hgt")
      }
    | None => Error("invalid hgt")
    }
  },
  // hcl
  (p: Passport.t<Passport.unvalidated>) => {
    let re = %re("/^#[0-9|a-f]{6}$/g")
    Js.Re.test_(re, p->Passport.hcl) ? Ok(p) : Error("invalid hcl")
  },
  // ecl
  (p: Passport.t<Passport.unvalidated>) => {
    ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]->Array.some(option =>
      option === p->Passport.ecl
    )
      ? Ok(p)
      : Error("invalid ecl")
  },
  // pid
  (p: Passport.t<Passport.unvalidated>) => {
    let re = %re("/^[0-9]{9}$/g")
    Js.Re.test_(re, p->Passport.pid) ? Ok(p) : Error("invalid pid")
  },
  // cid는 필요 없음
}

let checkValid = (passport, ~validateFns) =>
  validateFns->List.map(f => f(passport))->List.every(result => result->Result.isOk)

let passports =
  input
  ->Js.String2.split("\n\n")
  ->Array.map(Passport.make(~extractFn=Passport.extractFnByRe(re), ~init=Passport.empty))

// part1
let part1 =
  passports
  ->Array.reduce(list{}, (validPassports, passport) => {
    passport->checkValid(~validateFns=checkValidFns1)
      ? validPassports->List.add(Passport.validate(passport))
      : validPassports
  })
  ->List.length

part1->Js.log

// part2
let part2 =
  passports
  ->Array.reduce(list{}, (validPassports, passport) => {
    passport->checkValid(~validateFns=checkValidFns2)
      ? validPassports->List.add(Passport.validate(passport))
      : validPassports
  })
  ->List.length

part2->Js.log
