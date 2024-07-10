// let twoSum = (numbers: array<int>, target: int): array<int> => {
//   let table = Map.make()

//   let rec helper = (current: int): array<int> => {
//     let num = switch numbers->Array.get(current) {
//     | Some(n) => n
//     | None => -1
//     }

//     switch table->Map.get(target - num) {
//     | Some(index) => [index, current]
//     | None => {
//         table->Map.set(num, current)
//         helper(current + 1)
//       }
//     }
//   }

//   helper(0)
// }

let twoSum = (numbers: array<int>, target: int): array<int> => {
  let clone = numbers->Array.map(v => v)
  clone->Array.sort((a, b) => float(a - b))

  let rec helper = (left: int, right: int): array<int> => {
    let leftNum = switch clone->Array.get(left) {
    | None => 0
    | Some(number) => number
    }

    let rightNum = switch clone->Array.get(right) {
    | None => 0
    | Some(number) => number
    }

    let diff = leftNum + rightNum - target
    diff > 0 ? helper(left, right - 1) : diff < 0 ? helper(left + 1, right) : [left, right]
  }

  helper(0, Array.length(numbers) - 1)
}

let numbers = [1, 3, 7]
let result = twoSum(numbers, 8)
Console.log2("twoSum [1,3,7] 8", result)

let numbers2 = [-20, -9, 0, 1, 4, 6, 7]
let result2 = twoSum(numbers2, -14)
Console.log2("twoSum [-20, -9, 0, 1, 4, 6, 7] -14", result2)

let numbers3 = [2, 7, 11, 15]
let result3 = twoSum(numbers3, 9)
Console.log2("twoSum [2, 7, 11, 15] 9", result3)
