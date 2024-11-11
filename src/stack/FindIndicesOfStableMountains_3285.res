// T(n) = O(n)
// S(n) = O(n)

let findIndicesOfStableMountains = (heights: array<int>, threshold: int) => {
  let rec loop = (stableMountains: array<int>, stack: array<int>, index: int) => {
    switch index === Array.length(heights) {
    | true => stableMountains
    | false => {
        let currHeight = switch heights->Array.at(index) {
        | None => -1
        | Some(h) => h
        }
        let prevHeight = switch stack->Array.at(-1) {
        | None => -1
        | Some(h) => h
        }
        let updatedStack = stack->Array.concat([currHeight])

        switch prevHeight < 0 {
        | true => loop(stableMountains, updatedStack, index + 1)
        | false =>
          prevHeight > threshold
            ? loop(stableMountains->Array.concat([index]), updatedStack, index + 1)
            : loop(stableMountains, updatedStack, index + 1)
        }
      }
    }
  }

  loop([], [], 0)
}

let n1 = [1, 2, 3, 4, 5]
let t1 = 2
let r1 = findIndicesOfStableMountains(n1, t1)
Console.log2("r1: ", r1) // [3, 4]

let n2 = [10, 1, 10, 1, 10]
let t2 = 3
let r2 = findIndicesOfStableMountains(n2, t2)
Console.log2("r2: ", r2) // [1, 3]

let n3 = [10, 1, 10, 1, 10]
let t3 = 10
let r3 = findIndicesOfStableMountains(n3, t3)
Console.log2("r3: ", r3) // []
