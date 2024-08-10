// T(n) = O(n)
// S(n) = O(n)
// @see https://leetcode.com/problems/dota2-senate/solutions/3483399/simple-diagram-explanation/

let dota2Senate = (senate: string) => {
  let rec makeQueues = (dires: array<int>, radiants: array<int>, index: int) => {
    switch index === String.length(senate) {
    | true => (dires, radiants)
    | false => {
        let senator = senate->String.charAt(index)

        senator === "D"
          ? makeQueues(dires->Array.concat([index]), radiants, index + 1)
          : makeQueues(dires, radiants->Array.concat([index]), index + 1)
      }
    }
  }

  let rec debate = (dires: array<int>, radiants: array<int>, roundPosition: int) => {
    let diresRemaining = Array.length(dires)
    let radiantsRemaining = Array.length(radiants)

    switch diresRemaining === 0 {
    | true => "Radiants"
    | false =>
      switch radiantsRemaining === 0 {
      | true => "Dires"
      | false => {
          let direPosition = switch dires->Array.at(0) {
          | None => -1
          | Some(n) => n
          }
          let radiantPosition = switch radiants->Array.at(0) {
          | None => -1
          | Some(n) => n
          }

          switch direPosition < 0 || radiantPosition < 0 {
          | true => "Cato"
          | false =>
            switch direPosition < radiantPosition {
            | true =>
              debate(
                // senator is ahead and wins and goes back to end of queue with roundPosition as their new position
                dires->Array.slice(~start=1, ~end=diresRemaining)->Array.concat([roundPosition]),
                // senator removed from debate
                radiants->Array.slice(~start=1, ~end=radiantsRemaining),
                roundPosition + 1,
              )
            // both positions cannot be equal
            | false =>
              debate(
                dires->Array.slice(~start=1, ~end=diresRemaining),
                radiants
                ->Array.slice(~start=1, ~end=radiantsRemaining)
                ->Array.concat([roundPosition]),
                roundPosition + 1,
              )
            }
          }
        }
      }
    }
  }

  let (dires, radiants) = makeQueues([], [], 0)
  debate(dires, radiants, String.length(senate))
}

let s1 = "RD"
let r1 = dota2Senate(s1)
Console.log2("r1: ", r1) // "Radiant"

let s2 = "RDD"
let r2 = dota2Senate(s2)
Console.log2("r2: ", r2) // "Dire"
