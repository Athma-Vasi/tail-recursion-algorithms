// T(n) = O(n^3)
// S(n) = O(1)

let minimumSumOfMountainTriplets_I = (nums: array<int>) => {
  let length = Array.length(nums)

  let rec leftSideLoop = (minSum: int, leftSideIdx: int) => {
    switch leftSideIdx === length {
    | true => minSum
    | false => {
        let leftSide = switch nums->Array.at(leftSideIdx) {
        | None => 0
        | Some(n) => n
        }

        let rec peakLoop = (minSum_: int, peakIdx: int) => {
          switch peakIdx === length {
          | true => minSum_
          | false => {
              let peak = switch nums->Array.at(peakIdx) {
              | None => 0
              | Some(n) => n
              }

              let rec rightSideLoop = (minSum__: int, rightSideIdx: int) => {
                switch rightSideIdx === length {
                | true => minSum__
                | false => {
                    let rightSide = switch nums->Array.at(rightSideIdx) {
                    | None => 0
                    | Some(n) => n
                    }

                    switch leftSide < peak && peak > rightSide {
                    | true => {
                        let sum = leftSide + peak + rightSide
                        rightSideLoop(minSum__ < sum ? minSum__ : sum, rightSideIdx + 1)
                      }
                    | false => rightSideLoop(minSum__, rightSideIdx + 1)
                    }
                  }
                }
              }

              let minSum__ = rightSideLoop(minSum_, peakIdx + 1)
              peakLoop(minSum_ < minSum__ ? minSum_ : minSum__, peakIdx + 1)
            }
          }
        }

        let minSum_ = peakLoop(minSum, leftSideIdx + 1)
        leftSideLoop(minSum < minSum_ ? minSum : minSum_, leftSideIdx + 1)
      }
    }
  }

  let minSum = leftSideLoop(Int32.max_int, 0)
  switch minSum === Int32.max_int {
  | true => -1
  | false => minSum
  }
}

let n1 = [8, 6, 1, 5, 3]
let r1 = minimumSumOfMountainTriplets_I(n1)
Console.log2("[8,6,1,5,3]", r1) // 9

let n2 = [5, 4, 8, 7, 10, 2]
let r2 = minimumSumOfMountainTriplets_I(n2)
Console.log2("[5,4,8,7,10,2]", r2) // 13

let n3 = [6, 5, 4, 3, 4, 5]
let r3 = minimumSumOfMountainTriplets_I(n3)
Console.log2("[6,5,4,3,4,5]", r3) // -1
