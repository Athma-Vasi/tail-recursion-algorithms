// T(n) = O(n)
// S(n) = O(1)

let shortestDistanceToTargetStringInACircularArray = (
  words: array<string>,
  target: string,
  startIndex: int,
) => {
  let length = Array.length(words)

  let rec circularTraverse = (shortestDistance: int, leftIndex: int, rightIndex: int) => {
    switch leftIndex === startIndex || rightIndex === startIndex {
    | true => shortestDistance
    | false => {
        let leftDistance =
          leftIndex === length - 1 ? startIndex - leftIndex + 1 : leftIndex - startIndex
        let absLeftDistance = leftDistance < 0 ? leftDistance * -1 : leftDistance
        let rightDistance = rightIndex === 0 ? rightIndex + 1 + startIndex : rightIndex - startIndex
        let absRightDistance = rightDistance < 0 ? rightDistance * -1 : rightDistance
        let leftWord = words->Array.at(leftIndex)->Option.mapOr("", w => w)
        let rightWord = words->Array.at(rightIndex)->Option.mapOr("", w => w)
        let newLeftIndex = leftIndex - 1 < 0 ? length - 1 : leftIndex - 1
        let newRightIndex = rightIndex + 1 === length ? 0 : rightIndex + 1

        switch (leftWord === target, rightWord === target) {
        | (true, true) =>
          circularTraverse(
            absLeftDistance < shortestDistance
              ? absLeftDistance
              : absRightDistance < shortestDistance
              ? absRightDistance
              : shortestDistance,
            newLeftIndex,
            newRightIndex,
          )
        | (false, true) =>
          circularTraverse(
            absRightDistance < shortestDistance ? absRightDistance : shortestDistance,
            newLeftIndex,
            newRightIndex,
          )
        | (true, false) =>
          circularTraverse(
            absLeftDistance < shortestDistance ? absLeftDistance : shortestDistance,
            newLeftIndex,
            newRightIndex,
          )
        | (false, false) => circularTraverse(shortestDistance, newLeftIndex, newRightIndex)
        }
      }
    }
  }

  let shortestDistance = circularTraverse(
    length + 1,
    startIndex - 1 < 0 ? length - 1 : startIndex - 1,
    startIndex + 1 === length ? 0 : startIndex + 1,
  )
  shortestDistance === length + 1 ? -1 : shortestDistance
}

let w1 = ["hello", "i", "am", "leetcode", "hello"]
let t1 = "hello"
let s1 = 1
let r1 = shortestDistanceToTargetStringInACircularArray(w1, t1, s1)
Console.log2("r1: hello ", r1) // 1

let w2 = ["a", "b", "leetcode"]
let t2 = "leetcode"
let s2 = 0
let r2 = shortestDistanceToTargetStringInACircularArray(w2, t2, s2)
Console.log2("r2: leetcode ", r2) // 1

let w3 = ["i", "eat", "leetcode"]
let t3 = "ate"
let s3 = 0
let r3 = shortestDistanceToTargetStringInACircularArray(w3, t3, s3)
Console.log2("r3: ate ", r3) // -1
