// T(n) = O(n)
// S(n) = O(n)

let twoSneakyNumbersOfDigitville = (nums: array<int>) => {
  let rec loop = (duplicates: array<int>, set: Set.t<int>, index: int) => {
    switch index === Array.length(nums) {
    | true => duplicates
    | false => {
        let num = switch nums->Array.at(index) {
        | None => -1
        | Some(n) => n
        }

        switch set->Set.has(num) {
        | true => loop(duplicates->Array.concat([num]), set, index + 1)
        | false => {
            set->Set.add(num)
            loop(duplicates, set, index + 1)
          }
        }
      }
    }
  }

  loop([], Set.make(), 0)
}

let n1 = [0, 1, 1, 0]
let r1 = twoSneakyNumbersOfDigitville(n1)
Console.log2("r1: ", r1) // [1, 0]

let n2 = [0, 3, 2, 1, 3, 2]
let r2 = twoSneakyNumbersOfDigitville(n2)
Console.log2("r2: ", r2) // [2, 3]

let n3 = [7, 1, 5, 4, 3, 4, 6, 0, 9, 5, 8, 2]
let r3 = twoSneakyNumbersOfDigitville(n3)
Console.log2("r3: ", r3) // [4, 5]
