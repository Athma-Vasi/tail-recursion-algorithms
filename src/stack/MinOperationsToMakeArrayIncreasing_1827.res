let minOperationsToMakeArrayIncreasing = (nums: array<int>) => {
  let rec loop = (stack: array<int>, opsCount: int, index: int) => {
    switch index === Array.length(nums) {
    | true => opsCount
    | false => {
        let currentNum = switch nums->Array.at(index) {
        | None => Int32.min_int
        | Some(num) => num
        }
        let previousNum = switch stack->Array.at(-1) {
        | None => Int32.min_int + 1
        | Some(num) => num
        }

        switch previousNum < currentNum {
        | true => loop(stack->Array.concat([currentNum]), opsCount, index + 1)
        | false => {
            let ops = previousNum - currentNum + 1
            loop(stack->Array.concat([previousNum + 1]), opsCount + ops, index + 1)
          }
        }
      }
    }
  }

  loop([], 0, 0)
}

let n1 = [1, 1, 1]
let r1 = minOperationsToMakeArrayIncreasing(n1)
Console.log2("[1,1,1]", r1)

let n2 = [1, 5, 2, 4, 1]
let r2 = minOperationsToMakeArrayIncreasing(n2)
Console.log2("[1,5,2,4,1]", r2)

let n3 = [8]
let r3 = minOperationsToMakeArrayIncreasing(n3)
Console.log2("[8]", r3)
