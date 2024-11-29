// T(n) = O(n^3)
// S(n) = O(1)

let outerMaxOfAnOrderedTriplet_I = (nums: array<int>) => {
  let length = Array.length(nums)

  let rec outerLoop = (outerMax: int, outerIndex: int) => {
    switch outerIndex === length {
    | true => outerMax
    | false => {
        let outerNum = switch nums->Array.at(outerIndex) {
        | None => 0
        | Some(n) => n
        }

        let rec middleLoop = (middleMax: int, middleIndex: int) => {
          switch middleIndex === length {
          | true => middleMax
          | false => {
              let middleNum = switch nums->Array.at(middleIndex) {
              | None => 0
              | Some(n) => n
              }

              let rec innerLoop = (innerMax: int, innerIndex: int) => {
                switch innerIndex === length {
                | true => innerMax
                | false => {
                    let innerNum = switch nums->Array.at(innerIndex) {
                    | None => 0
                    | Some(n) => n
                    }
                    let valueOfTriplet = (outerNum - middleNum) * innerNum

                    innerLoop(innerMax > valueOfTriplet ? innerMax : valueOfTriplet, innerIndex + 1)
                  }
                }
              }

              let innerMax = innerLoop(middleMax, middleIndex + 1)
              middleLoop(middleMax > innerMax ? middleMax : innerMax, middleIndex + 1)
            }
          }
        }

        let middleMax = middleLoop(outerMax, outerIndex + 1)
        outerLoop(outerMax > middleMax ? outerMax : middleMax, outerIndex + 1)
      }
    }
  }

  let maxValueOfTriplet = outerLoop(Int32.min_int, 0)
  maxValueOfTriplet > 0 ? maxValueOfTriplet : 0
}

let n1 = [12, 6, 1, 2, 7]
let r1 = outerMaxOfAnOrderedTriplet_I(n1)
Console.log2("r1: ", r1) // 77

let n2 = [1, 10, 3, 4, 19]
let r2 = outerMaxOfAnOrderedTriplet_I(n2)
Console.log2("r2: ", r2) // 133

let n3 = [1, 2, 3]
let r3 = outerMaxOfAnOrderedTriplet_I(n3)
Console.log2("r3: ", r3) // 0
