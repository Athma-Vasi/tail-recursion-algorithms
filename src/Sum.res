let sumRecursive = (numbers: array<int>): int => {
  let rec helper = (accumulator: int, numbers: array<int>): int => {
    let length = Array.length(numbers)

    switch length {
    | 0 => accumulator
    | _ => {
        let head = switch numbers->Array.get(0) {
        | None => 0
        | Some(number) => number
        }
        let tail = numbers->Array.slice(~start=1, ~end=length)

        helper(head + accumulator, tail)
      }
    }
  }

  helper(0, numbers)
}

let sumListRecursive = (numbers: list<int>): int => {
  let rec helper = (accumulator: int, numbers: list<int>): int => {
    let length = List.length(numbers)

    switch length {
    | 0 => accumulator
    | _ => {
        let head = switch List.head(numbers) {
        | None => 0
        | Some(number) => number
        }

        let tail = switch List.tail(numbers) {
        | None => List.make(~length=0, -1)
        | Some(rest) => rest
        }

        helper(head + accumulator, tail)
      }
    }
  }

  helper(0, numbers)
}

let sumIterative = (numbers: array<int>): int => {
  Array.reduce(numbers, 0, (accumulator, number) => accumulator + number)
}

let numbers = [1, 2, 3, 4, 5, 7, 8, 9, 2]

let result = sumRecursive(numbers)
Console.log2("sum", result)

let result2 = sumIterative(numbers)
Console.log2("sum", result2)

let numbersList = List.fromArray(numbers)
Console.log2("numbersList", numbersList)
let result3 = sumListRecursive(numbersList)
Console.log2("sum", result3)
