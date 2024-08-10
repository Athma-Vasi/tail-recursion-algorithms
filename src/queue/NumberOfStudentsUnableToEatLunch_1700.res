// T(n) = O(n)
// S(n) = O(n)

let numberOfStudentsUnableToEatLunch = (students: array<int>, sandwiches: array<int>) => {
  let rec countPreferences = (prefersZeroesCount: int, prefersOnesCount: int, index: int) => {
    let preference = switch students->Array.at(index) {
    | None => -1
    | Some(n) => n
    }

    index === Array.length(students)
      ? (prefersZeroesCount, prefersOnesCount)
      : preference === 0
      ? countPreferences(prefersZeroesCount + 1, prefersOnesCount, index + 1)
      : countPreferences(prefersZeroesCount, prefersOnesCount + 1, index + 1)
  }

  let rec pickSandwiches = (students, sandwiches, ~prefersZeroesCount, ~prefersOnesCount) => {
    let studentsLength = Array.length(students)
    let sandwichesLength = Array.length(sandwiches)

    switch sandwichesLength === 0 || studentsLength === 0 {
    | true => studentsLength
    | false => {
        let preference = switch students->Array.at(0) {
        | None => -1
        | Some(p) => p
        }
        let sandwich = switch sandwiches->Array.at(0) {
        | None => -1
        | Some(s) => s
        }

        switch preference < 0 || sandwich < 0 {
        | true => -1
        | false =>
          switch preference === 0 && sandwich === 0 {
          | true =>
            pickSandwiches(
              students->Array.slice(~start=1, ~end=studentsLength),
              sandwiches->Array.slice(~start=1, ~end=sandwichesLength),
              ~prefersZeroesCount=prefersZeroesCount - 1,
              ~prefersOnesCount,
            )
          | false =>
            switch preference === 1 && sandwich === 1 {
            | true =>
              pickSandwiches(
                students->Array.slice(~start=1, ~end=studentsLength),
                sandwiches->Array.slice(~start=1, ~end=sandwichesLength),
                ~prefersZeroesCount,
                ~prefersOnesCount=prefersOnesCount - 1,
              )
            | false =>
              switch prefersZeroesCount === 0 || prefersOnesCount === 0 {
              | true => studentsLength
              | false =>
                pickSandwiches(
                  students
                  ->Array.slice(~start=1, ~end=studentsLength)
                  ->Array.concat([preference]),
                  sandwiches,
                  ~prefersZeroesCount,
                  ~prefersOnesCount,
                )
              }
            }
          }
        }
      }
    }
  }

  let (prefersZeroesCount, prefersOnesCount) = countPreferences(0, 0, 0)
  pickSandwiches(students, sandwiches, ~prefersZeroesCount, ~prefersOnesCount)
}

let s1 = [1, 1, 0, 0]
let s11 = [0, 1, 0, 1]
let r1 = numberOfStudentsUnableToEatLunch(s1, s11)
Console.log2("r1: ", r1) // 0

let s2 = [1, 1, 1, 0, 0, 1]
let s22 = [1, 0, 0, 0, 1, 1]
let r2 = numberOfStudentsUnableToEatLunch(s2, s22)
Console.log2("r2: ", r2) // 3
