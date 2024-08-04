// T(n) = O(log n)
// S(n) = O(1)

let binarySearch = (nums: array<int>, target: int) => {
  let rec search = (leftIndex: int, rightIndex: int) => {
    switch leftIndex > rightIndex {
    | true => -1
    | false => {
        let middleIndex = leftIndex + (rightIndex - leftIndex) / 2
        let middleNum = switch nums->Array.at(middleIndex) {
        | None => Int32.min_int
        | Some(num) => num
        }

        middleNum === Int32.min_int
          ? -1
          : middleNum === target
          ? middleIndex
          : middleNum > target
          ? search(leftIndex, middleIndex - 1)
          : search(middleIndex + 1, rightIndex)
      }
    }
  }
  let length = Array.length(nums)

  length === 0 ? -1 : search(0, length - 1)
}

let n1 = [-1, 0, 3, 5, 9, 12]
let t1 = 9
let r1 = binarySearch(n1, t1)
Console.log2("r1: ", r1)

let n2 = [-1, 0, 3, 5, 9, 12]
let t2 = 2
let r2 = binarySearch(n2, t2)
Console.log2("r2: ", r2)

let n3 = [5]
let t3 = 5
let r3 = binarySearch(n3, t3)
Console.log2("r3: ", r3)

let n4 = [-1, 0, 3, 5, 9, 12, 13, 15, 17, 19, 23, 29]
let t4 = 18
let r4 = binarySearch(n4, t4)
Console.log2("r4: ", r4)
