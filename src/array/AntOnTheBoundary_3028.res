// T(n) = O(n)
// S(n) = O(1)

let antOnTheBoundary = (nums: array<int>) => {
  let rec travel1D = (count: int, position: int, index: int) => {
    let newCount = position === 0 && index !== 0 ? count + 1 : count

    switch index === Array.length(nums) {
    | true => newCount
    | false => {
        let num = switch nums->Array.at(index) {
        | None => -11
        | Some(n) => n
        }

        travel1D(newCount, position + num, index + 1)
      }
    }
  }

  travel1D(0, 0, 0)
}

let n1 = [2, 3, -5]
let r1 = antOnTheBoundary(n1)
Console.log2("r1: ", r1)

let n2 = [3, 2, -3, -4]
let r2 = antOnTheBoundary(n2)
Console.log2("r2: ", r2)
