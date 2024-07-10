let duplicateZeroes = (nums: array<int>): array<int> => {
  let length = Array.length(nums)

  let rec loop = (accumulator: array<int>, index: int, queue: array<int>) => {
    let currentNum = switch nums->Array.get(index) {
    | None => Int32.min_int
    | Some(num) => num
    }

    let newQueue = queue->Array.concat(currentNum === 0 ? [0, 0] : [currentNum])
    let popped = switch Array.shift(newQueue) {
    | None => Int32.min_int
    | Some(num) => num
    }
    accumulator->Array.set(index, popped)

    index === length - 1 ? accumulator : loop(accumulator, index + 1, newQueue)
  }

  loop(nums, 0, [])
}

let nums1 = [1, 0, 2, 3, 0, 4, 5, 0]
let result1 = duplicateZeroes(nums1)
Console.log2("nums1", result1)
