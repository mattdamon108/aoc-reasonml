open Belt

exception ParseError

let inputTest = Node_fs.readFileAsUtf8Sync("input/2020/day04test")->Js.String2.trim(_)

let input = Node_fs.readFileAsUtf8Sync("input/2020/day04")->Js.String2.trim(_)

module type Passport = {
  type unvalidated
  type validated
  type t<'a>

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

  let extract = raw => {
    let re = %re("/(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)/g")

    let rec extractor = m => {
      switch Js.Re.exec_(re, raw) {
      | Some(result) =>
        let captured = result->Js.Re.captures->Array.keepMap(Js.Nullable.toOption)

        switch (captured->Array.get(1), captured->Array.get(2)) {
        | (Some(k), Some(v)) => extractor(m->Map.String.set(k, v))
        | (_, _) => raise(ParseError)
        }
      | None => m
      }
    }

    extractor(Map.String.empty)
  }

  let make = raw => {
    let checkValidFns = list{
      // 모든 field가 있는 지 체크
      m =>
        ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]->Array.every(field =>
          m->Map.String.has(field)
        ),
      // byr, iyr, eyr 항목의 값이 숫자인지 확인
      m =>
        ["byr", "iyr", "eyr"]
        ->Array.keepMap(field =>
          m->Map.String.get(field)->Option.flatMap(value => value->Int.fromString)
        )
        ->Array.length === 3,
    }

    let extracted = raw->extract
    let isValid = checkValidFns->List.every(f => f(extracted))

    isValid
      ? {
          byr: extracted
          ->Map.String.get("byr")
          ->Option.flatMap(value => value->Int.fromString)
          ->Option.getExn,
          iyr: extracted
          ->Map.String.get("iyr")
          ->Option.flatMap(value => value->Int.fromString)
          ->Option.getExn,
          eyr: extracted
          ->Map.String.get("eyr")
          ->Option.flatMap(value => value->Int.fromString)
          ->Option.getExn,
          hgt: extracted->Map.String.getExn("hgt"),
          hcl: extracted->Map.String.getExn("hcl"),
          ecl: extracted->Map.String.getExn("ecl"),
          pid: extracted->Map.String.getExn("pid"),
          cid: extracted->Map.String.get("cid"),
        }->Some
      : None
  }

  let validate = passport => {
    let checkValidFns = list{
      // byr
      (p: t<unvalidated>) => 1920 <= p.byr && p.byr <= 2002 ? Ok(true) : Error("no byr"),
      // iyr
      (p: t<unvalidated>) => 2010 <= p.iyr && p.iyr <= 2020 ? Ok(true) : Error("no iyr"),
      // eyr
      (p: t<unvalidated>) => 2020 <= p.eyr && p.eyr <= 2030 ? Ok(true) : Error("no eyr"),
      // hgt
      (p: t<unvalidated>) => {
        let re = %re("/^(\d+)(cm|in)$/g")
        switch Js.Re.exec_(re, p.hgt) {
        | Some(result) =>
          let captured = result->Js.Re.captures->Array.keepMap(Js.Nullable.toOption)

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
        Js.Re.test_(re, p.hcl) ? Ok(true) : Error("invalid hcl")
      },
      // ecl
      (p: t<unvalidated>) => {
        ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]->Array.some(option => option === p.ecl)
          ? Ok(true)
          : Error("invalid ecl")
      },
      // pid
      (p: t<unvalidated>) => {
        let re = %re("/^[0-9]{9}$/g")
        Js.Re.test_(re, p.pid) ? Ok(true) : Error("invalid pid")
      },
      // cid는 필요 없음
    }

    let isValid =
      checkValidFns->List.map(f => f(passport))->List.every(result => result->Result.isOk)
    isValid ? passport->Some : None
  }
}

// part1
let unvalidatedPassports = input->Js.String2.split("\n\n")->Array.keepMap(Passport.make)

unvalidatedPassports->Array.length->Js.log

// part2
let validatedPassports = unvalidatedPassports->Array.keepMap(Passport.validate)

validatedPassports->Array.length->Js.log
