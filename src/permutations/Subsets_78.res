// T(n) = O(2^n)
// S(n) = O(2^n)
// @see https://leetcode.com/problems/subsets/solutions/5188149/explained-backtracking-iterative-and-recursive-approach-beats-90/

let subsets = (nums: array<int>) => {
  let numLength = Array.length(nums)

  let rec findPowerSet = (powerSet: array<array<int>>, numIndex: int) => {
    switch numIndex === numLength {
    | true => powerSet
    | false => {
        let num = switch nums->Array.at(numIndex) {
        | None => 0
        | Some(n) => n
        }
        let powerLength = Array.length(powerSet)

        Console.log("\n")
        Console.log("--findPowerSet--")
        Console.log2("powerSet: ", powerSet)
        Console.log2("numIndex: ", numIndex)
        Console.log2("num: ", num)
        Console.log2("powerLength: ", powerLength)

        let rec subsetLoop = (subset: array<array<int>>, subIndex: int) => {
          switch subIndex === powerLength {
          | true => subset
          | false => {
              let currSubset = switch powerSet->Array.at(subIndex) {
              | None => [num]
              | Some(set) => set->Array.concat([num])
              }

              Console.log("\n")
              Console.log("--subsetLoop--")
              Console.log2("subset: ", subset)
              Console.log2("subIndex: ", subIndex)
              Console.log2("currSubset: ", currSubset)

              subsetLoop(subset->Array.concat([currSubset]), subIndex + 1)
            }
          }
        }

        let subset = subsetLoop([], 0)

        Console.log("\n")
        Console.log("--findPowerSet--")
        Console.log2("subset: ", subset)

        findPowerSet(powerSet->Array.concat(subset), numIndex + 1)
      }
    }
  }

  findPowerSet([[]], 0)
}

let n1 = [1, 2, 3]
let r1 = subsets(n1)
Console.log2("r1: ", r1) // [ [], [ 1 ], [ 2 ], [ 1, 2 ], [ 3 ], [ 1, 3 ], [ 2, 3 ], [ 1, 2, 3 ] ]

let n2 = [0]
let r2 = subsets(n2)
Console.log2("r2: ", r2) // [ [], [ 0 ] ]
