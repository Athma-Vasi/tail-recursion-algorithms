let removeDuplicates = (nums: array<int>) => {
  let length = Array.length(nums)

  let rec loop = (accumulator: array<int>, leftIndex: int, rightIndex: int) => {
    let leftNum = switch nums->Array.get(leftIndex) {
    | None => Int32.min_int
    | Some(num) => num
    }
    let rightNum = switch nums->Array.get(rightIndex) {
    | None => Int32.min_int
    | Some(num) => num
    }

    switch rightIndex === length {
    | true => accumulator
    | false =>
      switch leftNum === rightNum {
      | true => loop(accumulator, leftIndex, rightIndex + 1)
      | false => {
          accumulator->Array.set(leftIndex + 1, rightNum)
          loop(accumulator, leftIndex + 1, rightIndex + 1)
        }
      }
    }
  }

  loop(nums, 0, 1)
}

let n1 = [1, 1, 2]
let r1 = removeDuplicates(n1)
Console.log2("[1,1,2]", r1)

let n2 = [0, 0, 1, 1, 1, 2, 2, 3, 3, 4]
let r2 = removeDuplicates(n2)
Console.log2("[0,0,1,1,1,2,2,3,3,4]", n2)
