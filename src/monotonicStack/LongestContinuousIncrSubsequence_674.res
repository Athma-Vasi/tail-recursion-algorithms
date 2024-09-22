let longestContinuousIncrSubsequence = (nums: array<int>) => {
  let rec loop = (longest: int, monoIncrStack: array<int>, index: int) => {
    switch index === Array.length(nums) {
    | true => longest
    | false => {
        let curr = switch nums->Array.at(index) {
        | None => Int32.min_int
        | Some(n) => n
        }

        let prev = switch monoIncrStack->Array.at(-1) {
        | None => Int32.max_int
        | Some(n) => n
        }

        switch curr < prev {
        | true => {
            let length = Array.length(monoIncrStack)
            loop(longest > length ? longest : length, index === 0 ? [curr] : [], index + 1)
          }
        | false => loop(longest, monoIncrStack->Array.concat([curr]), index + 1)
        }
      }
    }
  }

  let longest = loop(Int32.min_int, [], 0)
  longest === 0 ? 1 : longest
}

let n1 = [1, 3, 5, 4, 7]
let r1 = longestContinuousIncrSubsequence(n1)
Console.log2("r1: ", r1)

let n2 = [2, 2, 2, 2, 2]
let r2 = longestContinuousIncrSubsequence(n2)
Console.log2("r2: ", r2)
