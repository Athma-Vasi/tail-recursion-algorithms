let leadersInAnArray = (nums: array<int>) => {
  let rec loop = (leaders: array<int>, max: int, index: int) => {
    switch index < 0 {
    | true => leaders
    | false => {
        let num = switch nums->Array.at(index) {
        | None => Int32.min_int
        | Some(n) => n
        }

        num > max
          ? loop([num]->Array.concat(leaders), num, index - 1)
          : loop(leaders, max, index - 1)
      }
    }
  }

  loop([], Int32.min_int, Array.length(nums) - 1)
}

let n1 = [16, 17, 4, 3, 5, 2]
let r1 = leadersInAnArray(n1)
Console.log2("r1: ", r1)

let n2 = [1, 2, 3, 4, 5, 2]
let r2 = leadersInAnArray(n2)
Console.log2("r2: ", r2)
