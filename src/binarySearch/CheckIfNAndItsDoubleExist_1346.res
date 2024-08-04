// T(n) = O(n log n)
// S(n) = O(n)

let checkIfNAndItsDoubleExist = (arr: array<int>) => {
  let sorted = arr->Array.map(n => n)
  sorted->Array.sort((a, b) => float(a - b))

  let rec binarySearch = (leftIndex: int, rightIndex: int, target: int) => {
    let middleIndex = leftIndex + (rightIndex - leftIndex) / 2
    let middleN = switch sorted->Array.at(middleIndex) {
    | None => Int32.min_int
    | Some(num) => num
    }

    leftIndex > rightIndex || middleN === Int32.min_int
      ? false
      : middleN === target
      ? true
      : middleN > target
      ? binarySearch(leftIndex, middleIndex - 1, target)
      : binarySearch(middleIndex + 1, rightIndex, target)
  }

  let length = Array.length(arr)

  let rec loop = (isExistsSet: Set.t<bool>, index: int) => {
    let currentN = switch sorted->Array.at(index) {
    | None => Int32.min_int
    | Some(num) => num
    }
    let doubledN = currentN + currentN
    isExistsSet->Set.add(binarySearch(index + 1, length - 1, doubledN))

    index === length ? isExistsSet->Set.has(true) : loop(isExistsSet, index + 1)
  }

  loop(Set.make(), 0)
}

let a1 = [10, 2, 5, 3]
let r1 = checkIfNAndItsDoubleExist(a1) // true
Console.log2("r1: ", r1)

let a2 = [7, 1, 14, 11]
let r2 = checkIfNAndItsDoubleExist(a2) // true
Console.log2("r2: ", r2)

let a3 = [3, 1, 7, 11]
let r3 = checkIfNAndItsDoubleExist(a3) // false
Console.log2("r3: ", r3)
