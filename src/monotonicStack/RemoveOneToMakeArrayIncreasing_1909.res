// T(n) = O(n)
// S(n) = O(n)

let removeOneToMakeArrayIncreasing = (nums: array<int>) => {
  let length = Array.length(nums)

  let rec loop = (isMonoIncr: bool, monoIncrStack: array<int>, popCount: int, index: int) => {
    let stackLength = Array.length(monoIncrStack)

    switch index === length {
    | true => isMonoIncr
    | false => {
        let currentNum = switch nums->Array.at(index) {
        | None => Int32.min_int
        | Some(num) => num
        }

        let previousMax = switch monoIncrStack->Array.at(-1) {
        | None => Int32.min_int
        | Some(num) => num
        }

        switch previousMax >= currentNum {
        | true =>
          popCount === 1
            ? loop(
                false,
                monoIncrStack->Array.slice(~start=0, ~end=stackLength - 1),
                popCount,
                index,
              )
            : loop(
                isMonoIncr === false ? false : true,
                monoIncrStack->Array.slice(~start=0, ~end=stackLength - 1),
                popCount + 1,
                index,
              )

        | false => loop(isMonoIncr, monoIncrStack->Array.concat([currentNum]), popCount, index + 1)
        }
      }
    }
  }

  loop(true, [], 0, 0)
}

let n1 = [1, 2, 10, 5, 7]
let r1 = removeOneToMakeArrayIncreasing(n1)
Console.log2("r1: ", r1)

let n2 = [2, 3, 1, 2]
let r2 = removeOneToMakeArrayIncreasing(n2)
Console.log2("r2: ", r2)

let n3 = [1, 1, 1]
let r3 = removeOneToMakeArrayIncreasing(n3)
Console.log2("r3: ", r3)
