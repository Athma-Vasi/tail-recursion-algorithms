// T(n) = O(n)
// S(n) = O(n)

let numberOfSeniorCitizens = (details: array<string>) => {
  let rec collect = (ages: array<string>, index: int) => {
    switch index === Array.length(details) {
    | true => ages
    | false =>
      collect(
        ages->Array.concat([
          details
          ->Array.at(index)
          ->Option.mapOr(String.make(), detail => detail->String.slice(~start=11, ~end=13)),
        ]),
        index + 1,
      )
    }
  }

  collect([], 0)->Array.reduce(0, (acc, age) =>
    age->Int.fromString->Option.mapOr(0, a => a) > 60 ? acc + 1 : acc
  )
}

let d1 = ["7868190130M7522", "5303914400F9211", "9273338290F4010"]
let r1 = numberOfSeniorCitizens(d1)
Console.log2("r1: ", r1) // 2

let d2 = ["1313579440F2036", "2921522980M5644"]
let r2 = numberOfSeniorCitizens(d2)
Console.log2("r2: ", r2) // 0
