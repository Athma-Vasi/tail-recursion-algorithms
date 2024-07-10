// O(n^2)
// let findPivotIndex = (numbers: array<int>): int => {
//   let rec loop = (pointer: int) => {
//     let length = Array.length(numbers)
//     let leftNumbers = numbers->Array.slice(~start=0, ~end=pointer)
//     let leftSum = leftNumbers->Array.reduce(0, (acc, num) => acc + num)
//     let rightNumbers = numbers->Array.slice(~start=pointer + 1, ~end=length)
//     let rightSum = rightNumbers->Array.reduce(0, (acc, num) => acc + num)

//     leftSum === rightSum ? pointer : pointer === length - 1 ? -1 : loop(pointer + 1)
//   }

//   loop(0)
// }

// O(n)
let findPivotIndex = (numbers: array<int>): int => {
  let length = Array.length(numbers)
  let total = numbers->Array.reduce(0, (acc, curr) => acc + curr)

  let rec loop = (pointer: int, leftSum: int): int => {
    let leftNum = switch numbers->Array.get(pointer - 1) {
    | None => 0
    | Some(num) => num
    }
    let currentNum = switch numbers->Array.get(pointer) {
    | None => 0
    | Some(num) => num
    }

    let newLeftSum = leftSum + leftNum
    let rightSum = total - currentNum - newLeftSum

    newLeftSum === rightSum ? pointer : pointer === length - 1 ? -1 : loop(pointer + 1, newLeftSum)
  }

  loop(0, 0)
}

let nums1 = [1, 7, 3, 6, 5, 6]
let result1 = findPivotIndex(nums1)
Console.log2("[1,7,3,6,5,6]", result1)
