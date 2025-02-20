// T(n) = O(n^2)
// S(n) = O(n)

let numberOfDistinctAverages = (nums: array<int>) => {
  let rec operation = (distinct: Set.t<float>, modified: array<int>) => {
    switch Array.length(modified) === 0 {
    | true => Set.size(distinct)
    | false => {
        let (min, minIdx) = modified->Array.reduceWithIndex((101, -1), (acc, num, idx) => {
          let (min, minIdx) = acc
          num < min ? (num, idx) : (min, minIdx)
        })
        let (max, maxIdx) = modified->Array.reduceWithIndex((0, -1), (acc, num, idx) => {
          let (max, maxIdx) = acc
          num > max ? (num, idx) : (max, maxIdx)
        })
        let filtered =
          modified
          ->Array.mapWithIndex((num, idx) => idx !== minIdx && idx !== maxIdx ? num : -1)
          ->Array.filter(num => num > 0)
        let avg = (Float.fromInt(min) +. Float.fromInt(max)) /. 2.0
        distinct->Set.add(avg)

        operation(distinct, filtered)
      }
    }
  }

  operation(Set.make(), nums)
}

let n1 = [4, 1, 4, 0, 3, 5]
let r1 = numberOfDistinctAverages(n1)
Console.log2("r1: ", r1) // 2

let n2 = [1, 100]
let r2 = numberOfDistinctAverages(n2)
Console.log2("r2: ", r2) // 1
