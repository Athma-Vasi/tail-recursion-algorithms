let maxAscendingSubArraySum = (nums: array<int>) => {
  let rec loop = (maxSum: int, monoIncrStack: array<int>, index: int) => {
    switch index === Array.length(nums) {
    | true => {
        let newSum = monoIncrStack->Array.reduce(0, (acc, curr) => acc + curr)
        newSum > maxSum ? newSum : maxSum
      }
    | false => {
        let currentNum = switch nums->Array.at(index) {
        | None => Int32.min_int
        | Some(num) => num
        }
        let previousNum = switch monoIncrStack->Array.at(-1) {
        | None => Int32.min_int + 1
        | Some(num) => num
        }

        switch currentNum < previousNum {
        | true => {
            let newSum = monoIncrStack->Array.reduce(0, (acc, curr) => acc + curr)
            loop(newSum > maxSum ? newSum : maxSum, [currentNum], index + 1)
          }
        | false => loop(maxSum, monoIncrStack->Array.concat([currentNum]), index + 1)
        }
      }
    }
  }

  loop(Int32.min_int, [], 0)
}

let n1 = [10, 20, 30, 5, 10, 50]
let r1 = maxAscendingSubArraySum(n1) // 65
Console.log2("r1: ", r1)

let n2 = [10, 20, 30, 40, 50]
let r2 = maxAscendingSubArraySum(n2) // 150
Console.log2("r2: ", r2)

let n3 = [12, 17, 15, 13, 10, 11, 12]
let r3 = maxAscendingSubArraySum(n3) // 33
Console.log2("r3: ", r3)
