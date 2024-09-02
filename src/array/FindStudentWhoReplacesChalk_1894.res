// T(n) = O(n)
// S(n) = O(1)

let findStudentWhoReplacesChalk = (chalksRequired: array<int>, totalChalk: int) => {
  let sum = chalksRequired->Array.reduce(0, (total, chalkRequired) => total + chalkRequired)

  let rec findRemainder = (chalkRemaining: int) => {
    chalkRemaining < sum ? chalkRemaining : findRemainder(chalkRemaining - sum)
  }

  let rec findStudent = (chalkRemaining: int, index: int) => {
    let chalkRequired = switch chalksRequired->Array.at(index) {
    | None => -1
    | Some(c) => c
    }

    chalkRemaining <= 0 || chalkRemaining < chalkRequired || chalkRequired < 0
      ? index
      : findStudent(chalkRemaining - chalkRequired, index + 1)
  }

  let remainder = findRemainder(totalChalk)
  findStudent(remainder, 0)
}

let c1 = [5, 1, 5]
let c2 = 22
let r1 = findStudentWhoReplacesChalk(c1, c2)
Console.log2("r1: ", r1)

let c3 = [3, 4, 1, 2]
let c4 = 25
let r2 = findStudentWhoReplacesChalk(c3, c4)
Console.log2("r2: ", r2)
