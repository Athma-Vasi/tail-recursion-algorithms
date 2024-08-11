let majorityElement = (nums: array<int>) => {
  // @see https://www.geeksforgeeks.org/boyer-moore-majority-voting-algorithm/

  let rec votingLoop = (count: int, index: int, candidate: int) => {
    switch index === Array.length(nums) {
    | true => candidate
    | false => {
        let num = switch nums->Array.at(index) {
        | None => Int32.min_int
        | Some(n) => n
        }

        switch num === candidate {
        | true => votingLoop(count + 1, index + 1, candidate)
        | false =>
          switch count === 0 {
          | true => votingLoop(1, index + 1, num)
          | false => votingLoop(count - 1, index + 1, candidate)
          }
        }
      }
    }

    // requires a final loop to confirm the candidate
    // not necessary in this problem as the prompt guarantees a majority element
  }

  let candidate = switch nums->Array.at(0) {
  | None => Int32.min_int
  | Some(n) => n
  }
  votingLoop(1, 1, candidate)
}

let n1 = [3, 2, 3]
let r1 = majorityElement(n1)
Console.log2("r1: ", r1) // 3

let n2 = [2, 2, 1, 1, 1, 2, 2]
let r2 = majorityElement(n2)
Console.log2("r2: ", r2) // 2
