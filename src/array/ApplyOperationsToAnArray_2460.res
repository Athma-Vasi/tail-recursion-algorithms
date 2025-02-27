// T(n) = O(n^2)
// S(n) = O(n^2)

let applyOperationsToAnArray = (nums: array<int>) => {
  let length = Array.length(nums)

  let rec operation = (modified: array<int>, leftIndex: int) => {
    switch leftIndex === length - 1 {
    | true => modified
    | false => {
        let leftNum = modified->Array.at(leftIndex)->Option.mapOr(-1, n => n)
        let rightNum = modified->Array.at(leftIndex + 1)->Option.mapOr(-1, n => n)

        switch leftNum === rightNum {
        | true => {
            let newModified =
              modified
              ->Array.mapWithIndex((n, index) => index === leftIndex ? leftNum * 2 : n)
              ->Array.mapWithIndex((n, index) => index === leftIndex + 1 ? 0 : n)

            operation(newModified, leftIndex + 1)
          }
        | false => operation(modified, leftIndex + 1)
        }
      }
    }
  }

  let (nonZeroes, zeroes) = operation(nums, 0)->Array.reduce(([], []), (acc, num) => {
    let (nonZeroes, zeroes) = acc
    num > 0 ? (nonZeroes->Array.concat([num]), zeroes) : (nonZeroes, zeroes->Array.concat([num]))
  })
  nonZeroes->Array.concat(zeroes)
}

let n1 = [1, 2, 2, 1, 1, 0]
let r1 = applyOperationsToAnArray(n1)
Console.log2("r1: ", r1) // [1, 4, 2, 0, 0, 0]

let n2 = [0, 1]
let r2 = applyOperationsToAnArray(n2)
Console.log2("r2: ", r2) // [1, 0]
