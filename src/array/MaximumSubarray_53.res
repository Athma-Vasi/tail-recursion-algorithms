// T(n) = O(n)
// S(n) = O(1)

let maximumSubarray = (nums: array<int>) => {
  // Kadane's algorithm
  let rec loop = (~max: int, ~sum: int, ~index: int) => {
    switch index === Array.length(nums) {
    | true => max
    | false => {
        let num = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }
        let newSum = sum + num

        loop(~max=max > newSum ? max : newSum, ~sum=newSum > 0 ? newSum : 0, ~index=index + 1)
      }
    }
  }

  loop(~max=Int32.min_int, ~sum=0, ~index=0)
}

let n1 = [-2, 1, -3, 4, -1, 2, 1, -5, 4]
let r1 = maximumSubarray(n1)
Console.log2("r1: ", r1) // 6

let n2 = [1]
let r2 = maximumSubarray(n2)
Console.log2("r2: ", r2) // 1
