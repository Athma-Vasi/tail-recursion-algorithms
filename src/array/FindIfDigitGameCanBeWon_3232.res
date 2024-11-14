// T(n) = O(n)
// S(n) = O(1)

let findIfDigitGameCanBeWon = (nums: array<int>) => {
  let rec loop = (singlesCount: int, doublesCount: int, index: int) => {
    switch index === Array.length(nums) {
    | true => singlesCount > doublesCount || doublesCount > singlesCount
    | false => {
        let num = switch nums->Array.at(index) {
        | None => -1
        | Some(n) => n
        }

        num < 10
          ? loop(singlesCount + num, doublesCount, index + 1)
          : loop(singlesCount, doublesCount + num, index + 1)
      }
    }
  }

  loop(0, 0, 0)
}

let n1 = [1, 2, 3, 4, 10]
let r1 = findIfDigitGameCanBeWon(n1)
Console.log2("r1: ", r1) // false

let n2 = [1, 2, 3, 4, 5, 14]
let r2 = findIfDigitGameCanBeWon(n2)
Console.log2("r2: ", r2) // true

let n3 = [5, 5, 5, 25]
let r3 = findIfDigitGameCanBeWon(n3)
Console.log2("r3: ", r3) // true
