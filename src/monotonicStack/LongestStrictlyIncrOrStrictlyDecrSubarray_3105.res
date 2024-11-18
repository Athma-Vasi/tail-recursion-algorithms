// T(n) = O(n)
// S(n) = O(n)

let longestStrictlyIncrOrStrictlyDecrSubarray = (nums: array<int>) => {
  let rec makeMonoIncrStack = (monoIncrStack: array<int>, nums: array<int>, index: int) => {
    switch index === Array.length(nums) {
    | true => monoIncrStack
    | false => {
        let prevNum = switch monoIncrStack->Array.at(-1) {
        | None => 0
        | Some(n) => n
        }
        let currNum = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }

        switch currNum > prevNum || prevNum === 0 {
        | true => makeMonoIncrStack(monoIncrStack->Array.concat([currNum]), nums, index + 1)
        | false => makeMonoIncrStack(monoIncrStack, nums, index + 1)
        }
      }
    }
  }

  let rec makeMonoDecrStack = (monoDecrStack: array<int>, nums: array<int>, index: int) => {
    switch index === Array.length(nums) {
    | true => monoDecrStack
    | false => {
        let prevNum = switch monoDecrStack->Array.at(-1) {
        | None => 0
        | Some(n) => n
        }
        let currNum = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }

        switch currNum < prevNum || prevNum === 0 {
        | true => makeMonoDecrStack(monoDecrStack->Array.concat([currNum]), nums, index + 1)
        | false => makeMonoDecrStack(monoDecrStack, nums, index + 1)
        }
      }
    }
  }

  let monoIncrStackLength = Array.length(makeMonoIncrStack([], nums, 0))
  let monoDecrStackLength = Array.length(makeMonoDecrStack([], nums, 0))
  monoIncrStackLength > monoDecrStackLength ? monoIncrStackLength : monoDecrStackLength
}

let n1 = [1, 4, 3, 3, 2]
let r1 = longestStrictlyIncrOrStrictlyDecrSubarray(n1)
Console.log2("r1: ", r1) // 2

let n2 = [3, 3, 3, 3]
let r2 = longestStrictlyIncrOrStrictlyDecrSubarray(n2)
Console.log2("r2: ", r2) // 1

let n3 = [3, 2, 1]
let r3 = longestStrictlyIncrOrStrictlyDecrSubarray(n3)
Console.log2("r3: ", r3) // 3
