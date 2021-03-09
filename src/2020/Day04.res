open Belt

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day04test")->Js.String2.trim(_)->Js.String2.split("\n")

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day04")->Js.String2.trim(_)->Js.String2.split("\n")

let parse = raw => {
  let rec parser = (data, parsed, buffer) =>
    if data->Array.length < 1 {
      parsed->Array.concat([buffer])
    } else {
      let line = data->Array.getExn(0)
      if line !== "" {
        let splitted =
          line
          ->Js.String2.trim
          ->Js.String2.split(" ")
          ->Array.map(item => item->Js.String2.split(":")->Array.getExn(0))
        let newBuffer = buffer->Array.concat(splitted)
        parser(data->Array.sliceToEnd(1), parsed, newBuffer)
      } else {
        let newParsed = parsed->Array.concat([buffer])
        parser(data->Array.sliceToEnd(1), newParsed, [])
      }
    }

  parser(raw, [], [])
}

let parse2 = raw => {
  let rec parser = (data, parsed, buffer) =>
    if data->Array.length < 1 {
      parsed->Array.concat([buffer])
    } else {
      let line = data->Array.getExn(0)
      if line !== "" {
        let splitted =
          line
          ->Js.String2.trim
          ->Js.String2.split(" ")
          ->Array.map(item => item->Js.String2.split(":"))
        let newBuffer = buffer->Array.concat(splitted)
        parser(data->Array.sliceToEnd(1), parsed, newBuffer)
      } else {
        let newParsed = parsed->Array.concat([buffer])
        parser(data->Array.sliceToEnd(1), newParsed, [])
      }
    }

  parser(raw, [], [])
}

let checkValid = passport =>
  passport->Array.some(field => field === "byr") &&
    (passport->Array.some(field => field === "iyr") &&
    (passport->Array.some(field => field === "eyr") &&
      (passport->Array.some(field => field === "hgt") &&
      (passport->Array.some(field => field === "hcl") &&
        (passport->Array.some(field => field === "ecl") &&
        passport->Array.some(field => field === "pid"))))))

@ocaml.doc("
 * byr 1920~2002
 * iyr 2010~2020
 * eyr 2020~2030
 * hgt cm => 150 ~ 193 , in => 59 ~ 76
 * hcl leading # and 6 character 0-9 or a-f
 * ecl amb, blu, brn, gry, grn, hzl, oth
 * pid nine digits, leading zeroes
 * cid optional
 ")
let checkValid2 = passport =>
  passport->Array.some(field => {
    let key = field->Array.getExn(0)
    if key === "byr" {
      let value = field->Array.getExn(1)->Int.fromString
      switch value {
      | Some(v) => 1920 <= v && v <= 2002
      | None => false
      }
    } else {
      false
    }
  }) &&
    (passport->Array.some(field => {
      let key = field->Array.getExn(0)
      if key === "iyr" {
        let value = field->Array.getExn(1)->Int.fromString
        switch value {
        | Some(v) => 2010 <= v && v <= 2020
        | None => false
        }
      } else {
        false
      }
    }) &&
    (passport->Array.some(field => {
      let key = field->Array.getExn(0)
      if key === "eyr" {
        let value = field->Array.getExn(1)->Int.fromString
        switch value {
        | Some(v) => 2020 <= v && v <= 2030
        | None => false
        }
      } else {
        false
      }
    }) &&
      (passport->Array.some(field => {
        let key = field->Array.getExn(0)
        if key === "hgt" {
          let value = field->Array.getExn(1)
          let cm = value->Js.String2.split("cm")
          let lengthCm = cm->Array.length
          let inch = value->Js.String2.split("in")
          let lengthInch = inch->Array.length

          if lengthCm > 1 {
            let valueCm = cm->Array.getExn(0)->Int.fromString
            switch valueCm {
            | Some(vc) => 150 <= vc && vc <= 193
            | None => false
            }
          } else if lengthInch > 1 {
            let valueIn = inch->Array.getExn(0)->Int.fromString
            switch valueIn {
            | Some(vi) => 59 <= vi && vi <= 76
            | None => false
            }
          } else {
            false
          }
        } else {
          false
        }
      }) &&
      (passport->Array.some(field => {
        let key = field->Array.getExn(0)
        if key === "hcl" {
          let re = %re("/^#[0-9|a-f]{6}$/g")
          let value = field->Array.getExn(1)
          Js.Re.test_(re, value)
        } else {
          false
        }
      }) &&
        (passport->Array.some(field => {
          let key = field->Array.getExn(0)
          if key === "ecl" {
            let value = field->Array.getExn(1)
            let optional = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
            optional->Array.some(option => option === value)
          } else {
            false
          }
        }) &&
        passport->Array.some(field => {
          let key = field->Array.getExn(0)
          if key === "pid" {
            let re = %re("/^[0-9]{9}$/g")
            let value = field->Array.getExn(1)
            Js.Re.test_(re, value)
          } else {
            false
          }
        }))))))

input
->parse
->Array.map(passport => passport->checkValid)
->Array.keep(valid => valid === true)
->Array.length
->Js.log

input
->parse2
->Array.map(passport => passport->checkValid2)
->Array.keep(valid => valid === true)
->Array.length
->Js.log
