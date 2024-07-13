// T(n) = O(n)
// S(n) = O(1)
// incorrect - need to add last num to accumulator

let squaresSortedArray = (nums: array<int>): array<int> => {
  let length = Array.length(nums)

  let rec loop = (accumulator: array<int>, accIndex: int, leftIndex: int, rightIndex: int) => {
    switch leftIndex < 0 || rightIndex === length {
    | true => accumulator
    | false => {
        let leftNum = switch nums->Array.get(leftIndex) {
        | None => Int32.min_int
        | Some(num) => num
        }
        let leftNumSquared = leftNum * leftNum
        let rightNum = switch nums->Array.get(rightIndex) {
        | None => Int32.min_int
        | Some(num) => num
        }
        let rightNumSquared = rightNum * rightNum

        Console.log("--loop")
        Console.log2("leftIndex", leftIndex)
        Console.log2("leftNum", leftNum)
        Console.log2("leftNumSquared", leftNumSquared)
        Console.log2("rightIndex", rightIndex)
        Console.log2("rightNum", rightNum)
        Console.log2("rightNumSquared", rightNumSquared)
        Console.log2("accIndex", accIndex)

        switch leftNumSquared < rightNumSquared {
        | true => {
            accumulator->Array.set(accIndex, leftNumSquared)
            Console.log2("accumulator", accumulator)

            loop(accumulator, accIndex + 1, leftIndex - 1, rightIndex)
          }
        | false => {
            accumulator->Array.set(accIndex, rightNumSquared)
            Console.log2("accumulator", accumulator)

            switch leftNumSquared > rightNumSquared {
            | true => loop(accumulator, accIndex + 1, leftIndex, rightIndex + 1)
            | false => loop(accumulator, accIndex + 1, leftIndex, rightIndex + 1)
            }
          }
        }
      }
    }
  }

  let isEvenLength = Float.mod(Int.toFloat(length), 2.0) == 0.0
  let leftIndex = switch isEvenLength {
  | true => Int.toFloat(length) /. 2.0 -. 1.0
  | false => Math.trunc(Int.toFloat(length) /. 2.0) -. 1.0
  }->Float.toInt
  let rightIndex = switch isEvenLength {
  | true => leftIndex + 1
  | false => leftIndex + 2
  }
  let middleNum = switch isEvenLength {
  | true => Int32.min_int
  | false =>
    switch nums->Array.get(leftIndex + 1) {
    | None => Int32.min_int
    | Some(num) => num
    }
  }
  let accumulator = Array.make(~length, Int32.min_int)
  switch isEvenLength {
  | true => ()
  | false => accumulator->Array.set(0, middleNum)
  }
  let accIndex = isEvenLength ? 0 : 1

  Console.log("--before loop")
  Console.log2("length", length)
  Console.log2("isEvenLength", isEvenLength)
  Console.log2("leftIndex", leftIndex)
  Console.log2("rightIndex", rightIndex)
  Console.log2("middleNum", middleNum)
  Console.log2("accumulator", accumulator)

  loop(accumulator, accIndex, leftIndex, rightIndex)
}

let n1 = [-4, -1, 0, 3, 10]
let r1 = squaresSortedArray(n1)
Console.log2("[-4,-1,0,3,10]", r1)
