let setMismatch = (nums: array<int>) => {
  let rec loop = (result: array<int>, index: int, stack: array<int>) => {
    switch index === Array.length(nums) {
    | true => result
    | false => {
        let curr = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }

        let prev = switch stack->Array.at(-1) {
        | None => Int32.min_int
        | Some(n) => n
        }

        switch curr === prev + 1 {
        | true => loop(result, index + 1, stack->Array.concat([curr]))
        | false =>
          loop(result->Array.concat([prev, curr + 1]), index + 1, stack->Array.concat([curr + 1]))
        }
      }
    }
  }

  let first = switch nums->Array.at(0) {
  | None => 0
  | Some(n) => n
  }

  loop([], 1, [first])
}

let n1 = [1, 2, 2, 4]
let r1 = setMismatch(n1)
Console.log2("r1: ", r1)

let n2 = [1, 1]
let r2 = setMismatch(n2)
Console.log2("r2: ", r2)
