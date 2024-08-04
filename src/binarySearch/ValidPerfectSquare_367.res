let validPerfectSquare = (num: int) => {
  let rec binarySearch = (left: int, right: int) => {
    let middle = left + (right - left) / 2
    let squared = middle * middle

    left > right
      ? false
      : squared === num
      ? true
      : squared < num
      ? binarySearch(middle + 1, right)
      : binarySearch(left, middle - 1)
  }

  num > 0 && binarySearch(1, num)
}

let n1 = 16
let r1 = validPerfectSquare(n1)
Console.log2("r1: ", r1)

let n2 = 14
let r2 = validPerfectSquare(n2)
Console.log2("r2: ", r2)
