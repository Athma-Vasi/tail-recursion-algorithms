// T(n) = O(n^3)
// S(n) = O(1)

let numberOfUnequalTripletsInArray = (nums: array<int>) => {
  let length = Array.length(nums)

  let rec outerLoop = (outerCount: int, outerIndex: int) => {
    switch outerIndex === length {
    | true => outerCount
    | false => {
        let outerNum = nums->Array.at(outerIndex)->Option.mapOr(-1, n => n)

        let rec middleLoop = (middleCount: int, middleIndex: int) => {
          switch middleIndex === length {
          | true => middleCount
          | false => {
              let middleNum = nums->Array.at(middleIndex)->Option.mapOr(-1, n => n)

              let rec innerLoop = (innerCount: int, innerIndex: int) => {
                switch innerIndex === length {
                | true => innerCount
                | false => {
                    let innerNum = nums->Array.at(innerIndex)->Option.mapOr(-1, n => n)

                    switch outerNum !== middleNum &&
                    middleNum !== innerNum &&
                    outerNum !== innerNum {
                    | true => innerLoop(innerCount + 1, innerIndex + 1)
                    | false => innerLoop(innerCount, innerIndex + 1)
                    }
                  }
                }
              }

              let innerCount = innerLoop(middleCount, middleIndex + 1)
              middleLoop(innerCount, middleIndex + 1)
            }
          }
        }

        let middleCount = middleLoop(outerCount, outerIndex + 1)
        outerLoop(middleCount, outerIndex + 1)
      }
    }
  }

  outerLoop(0, 0)
}

let n1 = [4, 4, 2, 4, 3]
let r1 = numberOfUnequalTripletsInArray(n1)
Console.log2("r1: ", r1) // 3

let n2 = [1, 1, 1, 1, 1]
let r2 = numberOfUnequalTripletsInArray(n2)
Console.log2("r2: ", r2) // 0
