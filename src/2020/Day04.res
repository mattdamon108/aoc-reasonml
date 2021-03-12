open Belt

exception ParseError

let inputTest = Node_fs.readFileAsUtf8Sync("input/2020/day04test")->Js.String2.trim(_)

let input = Node_fs.readFileAsUtf8Sync("input/2020/day04")->Js.String2.trim(_)

module type Passport = {
  type unvalidated
  type validated
  type t<'a>

  let byr: t<'a> => int
  let iyr: t<'a> => int
  let eyr: t<'a> => int
  let hgt: t<'a> => string
  let hcl: t<'a> => string
  let ecl: t<'a> => string
  let pid: t<'a> => string
  let cid: t<'a> => option<string>

  let make: string => option<t<unvalidated>>
  let validate: t<unvalidated> => option<t<validated>>
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

  let byr = (p: t<'a>) => p.byr
  let iyr = (p: t<'a>) => p.iyr
  let eyr = (p: t<'a>) => p.eyr
  let hgt = (p: t<'a>) => p.hgt
  let hcl = (p: t<'a>) => p.hcl
  let ecl = (p: t<'a>) => p.ecl
  let pid = (p: t<'a>) => p.pid
  let cid = (p: t<'a>) => p.cid

  let extractFnByRe = raw => {
    let re = %re("/(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)/g")

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

  let make = raw => {
    let checkValidFns = list{
      ((k, v)) =>
        switch v->Belt.Int.fromString {
        | Some(_) if k === "byr" => Ok(true)
        | _ => Error("parse error byr")
        },
      ((k, v)) =>
        switch v->Belt.Int.fromString {
        | Some(_) if k === "iyr" => Ok(true)
        | _ => Error("parse error iyr")
        },
      ((k, v)) =>
        switch v->Belt.Int.fromString {
        | Some(_) if k === "eyr" => Ok(true)
        | _ => Error("parse error eyr")
        },
      ((k, _)) => k === "hgt" ? Ok(true) : Error("parse error hgt"),
      ((k, _)) => k === "hcl" ? Ok(true) : Error("parse error hcl"),
      ((k, _)) => k === "ecl" ? Ok(true) : Error("parse error ecl"),
      ((k, _)) => k === "pid" ? Ok(true) : Error("parse error pid"),
      // cid는 필요 없음
    }

    let extractedPairs = raw->extractFnByRe
    let isValid =
      checkValidFns
      ->List.map(f => {
        extractedPairs->List.map(pair => pair->f)->List.keep(x => x->Result.isOk)->List.length > 0
          ? true
          : false
      })
      ->List.every(x => x)

    let emptyPassport: t<unvalidated> = {
      byr: 0,
      iyr: 0,
      eyr: 0,
      hgt: "",
      hcl: "",
      ecl: "",
      pid: "",
      cid: None,
    }

    isValid
      ? extractedPairs
        ->List.reduce(emptyPassport, (passport, field) => {
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
        ->Some
      : None
  }

  let validate = passport => {
    let checkValidFns = list{
      // byr
      (p: t<unvalidated>) => 1920 <= p->byr && p->byr <= 2002 ? Ok(true) : Error("no byr"),
      // iyr
      (p: t<unvalidated>) => 2010 <= p->iyr && p->iyr <= 2020 ? Ok(true) : Error("no iyr"),
      // eyr
      (p: t<unvalidated>) => 2020 <= p->eyr && p->eyr <= 2030 ? Ok(true) : Error("no eyr"),
      // hgt
      (p: t<unvalidated>) => {
        let re = %re("/^(\d+)(cm|in)$/g")
        switch Js.Re.exec_(re, p->hgt) {
        | Some(result) =>
          let captured =
            result->Js.Re.captures->Array.map(Js.Nullable.toOption)->Array.keepMap(x => x)
          switch (captured->Array.get(1)->Option.flatMap(Int.fromString), captured->Array.get(2)) {
          | (Some(h), Some(u)) =>
            switch u {
            | "cm" => 150 <= h && h <= 193 ? Ok(true) : Error("invalid hgt")
            | "in" => 59 <= h && h <= 76 ? Ok(true) : Error("invalid hgt")
            | _ => Error("invalid hgt")
            }
          | (_, _) => Error("invalid hgt")
          }
        | None => Error("invalid hgt")
        }
      },
      // hcl
      (p: t<unvalidated>) => {
        let re = %re("/^#[0-9|a-f]{6}$/g")
        Js.Re.test_(re, p->hcl) ? Ok(true) : Error("invalid hcl")
      },
      // ecl
      (p: t<unvalidated>) => {
        ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]->Array.some(option => option === p->ecl)
          ? Ok(true)
          : Error("invalid ecl")
      },
      // pid
      (p: t<unvalidated>) => {
        let re = %re("/^[0-9]{9}$/g")
        Js.Re.test_(re, p->pid) ? Ok(true) : Error("invalid pid")
      },
      // cid는 필요 없음
    }

    let isValid =
      checkValidFns->List.map(f => f(passport))->List.every(result => result->Result.isOk)
    isValid ? passport->Some : None
  }
}

// part1
let unvalidatedPassports =
  input->Js.String2.split("\n\n")->Array.map(Passport.make)->Array.keepMap(x => x)

unvalidatedPassports->Array.length->Js.log

// part2
let validatedPassports = unvalidatedPassports->Array.map(Passport.validate)->Array.keepMap(x => x)

validatedPassports->Array.length->Js.log
