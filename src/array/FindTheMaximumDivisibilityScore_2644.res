// T(n) = O(n)
// S(n) = O(n)

let findTheMaximumDivisibilityScore = (nums: array<int>, divisors: array<int>) => {
  let rec findDivisibilityScore = (scores: Map.t<int, int>, divisorsIndex: int) => {
    switch divisorsIndex === Array.length(divisors) {
    | true => scores
    | false => {
        let divisor = divisors->Array.at(divisorsIndex)->Option.mapOr(-1, d => d)

        let rec numsLoop = (scores_: Map.t<int, int>, numsIndex: int) => {
          switch numsIndex === Array.length(nums) {
          | true => scores_
          | false => {
              let num = nums->Array.at(numsIndex)->Option.mapOr(0, n => n)
              let remainder = Float.mod(Int.toFloat(num), Int.toFloat(divisor))
              let isDivisible = remainder === 0.0
              let score = scores_->Map.get(divisor)->Option.mapOr(0, s => s)
              scores_->Map.set(divisor, isDivisible ? score + 1 : score)

              numsLoop(scores_, numsIndex + 1)
            }
          }
        }

        numsLoop(scores, 0)->findDivisibilityScore(divisorsIndex + 1)
      }
    }
  }

  findDivisibilityScore(Map.make(), 0)
  ->Map.entries
  ->Array.fromIterator
  ->Array.toSorted(((k1, v1), (k2, v2)) =>
    v2 - v1 === 0 ? Int.compare(k1, k2) : Int.compare(v2, v1)
  )
  ->Array.at(0)
  ->Option.mapOr(-1, ((divisor, _score)) => divisor)
}

let n1 = [2, 9, 15, 50]
let d1 = [5, 3, 7, 2]
let r1 = findTheMaximumDivisibilityScore(n1, d1)
Console.log2("r1: ", r1) // 2

let n2 = [4, 7, 9, 3, 9]
let d2 = [5, 2, 3]
let r2 = findTheMaximumDivisibilityScore(n2, d2)
Console.log2("r2: ", r2) // 3

let n3 = [20, 14, 21, 10]
let d3 = [10, 16, 20]
let r3 = findTheMaximumDivisibilityScore(n3, d3)
Console.log2("r3: ", r3) // 10
