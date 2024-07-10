// T(n) = O(n)
// S(n) = O(1)

let product = (numbers: array<int>): int => {
  let rec helper = (accumulator: int, rest: array<int>) => {
    let length = Array.length(rest)

    switch length {
    | 0 => accumulator
    | _ => {
        let head = switch rest->Array.get(0) {
        | None => 1
        | Some(number) => number
        }

        let tail = rest->Array.slice(~start=1, ~end=length)

        helper(accumulator * head, tail)
      }
    }
  }

  helper(1, numbers)
}

let numbers = [1, 2, 3, 4, 5]
let result = product(numbers)
Console.log2("product", result)
