// T(n) = O(n)
// S(n) = O(n)

let maxNumberOfOpsWithTheSameScore_I = (nums: array<int>) => {
  let rec loop = (operations: int, scores: Set.t<int>, sliced: array<int>) => {
    let length = Array.length(sliced)

    switch Set.size(scores) > 1 {
    | true => operations - 1
    | false =>
      switch length < 2 {
      | true => operations
      | false => {
          let first = switch sliced->Array.at(0) {
          | None => 0
          | Some(n) => n
          }
          let second = switch sliced->Array.at(1) {
          | None => 0
          | Some(n) => n
          }
          scores->Set.add(first + second)

          loop(operations + 1, scores, sliced->Array.slice(~start=2, ~end=length))
        }
      }
    }
  }

  Array.length(nums) === 2 ? 1 : loop(0, Set.make(), nums)
}

let n1 = [3, 2, 1, 4, 5]
let r1 = maxNumberOfOpsWithTheSameScore_I(n1)
Console.log2("r1: ", r1) // 2

let n2 = [1, 5, 3, 3, 4, 1, 3, 2, 2, 3]
let r2 = maxNumberOfOpsWithTheSameScore_I(n2)
Console.log2("r2: ", r2) // 2

let n3 = [5, 3]
let r3 = maxNumberOfOpsWithTheSameScore_I(n3)
Console.log2("r3: ", r3) // 1
