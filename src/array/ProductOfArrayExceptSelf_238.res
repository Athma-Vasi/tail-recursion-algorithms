// T(n) = O(n)
// S(n) = O(n)

let productOfArrayExceptSelf = (nums: array<int>) => {
  let prefixProducts = nums->Array.reduceWithIndex([1], (acc, _curr, idx) => {
    switch idx < 1 {
    | true => acc
    | false => {
        let num = switch nums->Array.at(idx - 1) {
        | None => 0
        | Some(n) => n
        }
        let prev = switch acc->Array.at(-1) {
        | None => 0
        | Some(n) => n
        }

        acc->Array.concat([num * prev])
      }
    }
  })

  let suffixProducts = nums->Array.reduceRightWithIndex([1], (acc, _curr, idx) => {
    switch idx > Array.length(nums) - 2 {
    | true => acc
    | false => {
        let num = switch nums->Array.at(idx + 1) {
        | None => 0
        | Some(n) => n
        }
        let prev = switch acc->Array.at(0) {
        | None => 0
        | Some(n) => n
        }

        [num * prev]->Array.concat(acc)
      }
    }
  })

  prefixProducts->Array.reduceWithIndex([], (result, prefixProduct, idx) => {
    let suffixProduct = switch suffixProducts->Array.at(idx) {
    | None => 0
    | Some(n) => n
    }
    result->Array.concat([prefixProduct * suffixProduct])
  })
}

let n1 = [2, 3, 4, 5]
let r1 = productOfArrayExceptSelf(n1)
Console.log2("r1: ", r1) // [60, 40, 30, 24]

let n2 = [1, 2, 3, 4]
let r2 = productOfArrayExceptSelf(n2)
Console.log2("r2: ", r2) // [24, 12, 8, 6]

let n3 = [-1, 1, 0, -3, 3]
let r3 = productOfArrayExceptSelf(n3)
Console.log2("r3: ", r3) // [0, 0, 9, 0, 0]
