// T(n) = O(n)
// S(n) = O(1)

let minDistanceToTargetElement = (nums: array<int>, target: int, start: int) => {
  let rec loop = (minDistance: int, index: int) => {
    switch index === Array.length(nums) {
    | true => minDistance
    | false => {
        let num = switch nums->Array.at(index) {
        | None => Int32.max_int
        | Some(n) => n
        }

        switch num === target {
        | true => {
            let newMinDistance = index - start
            loop(newMinDistance < minDistance ? newMinDistance : minDistance, index + 1)
          }
        | false => loop(minDistance, index + 1)
        }
      }
    }
  }

  loop(Int32.max_int, 0)
}

let n1 = [1, 2, 3, 4, 5]
let t1 = 5
let s1 = 3
let r1 = minDistanceToTargetElement(n1, t1, s1)
Console.log2("r1: ", r1)

let n2 = [1]
let t2 = 1
let s2 = 0
let r2 = minDistanceToTargetElement(n2, t2, s2)
Console.log2("r2: ", r2)

let n3 = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
let t3 = 1
let s3 = 0
let r3 = minDistanceToTargetElement(n3, t3, s3)
Console.log2("r3: ", r3)
