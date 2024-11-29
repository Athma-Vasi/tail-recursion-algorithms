// T(n) = O(n)
// S(n) = O(n)

let lastVisitedIntegers = (nums: array<int>) => {
  let rec lastVisited = (answer: array<int>, seen: array<int>, k: int, index: int) => {
    switch index === Array.length(nums) {
    | true => answer
    | false => {
        let num = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }

        switch num > 0 {
        | true => lastVisited(answer, [num]->Array.concat(seen), 0, index + 1)
        | false => {
            let first = switch seen->Array.at(k) {
            | None => -1
            | Some(n) => n
            }

            lastVisited(answer->Array.concat([first]), seen, k + 1, index + 1)
          }
        }
      }
    }
  }

  lastVisited([], [], 0, 0)
}

let n1 = [1, 2, -1, -1, -1]
let r1 = lastVisitedIntegers(n1)
Console.log2("r1: ", r1) // [2, 1, -1]

let n2 = [1, -1, 2, -1, -1]
let r2 = lastVisitedIntegers(n2)
Console.log2("r2: ", r2) // [1, 2, 1]
