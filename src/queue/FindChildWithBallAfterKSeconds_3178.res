// T(n) = O(k)
// S(n) = O(1)

type direction = Left | Right

let findChildWithBallAfterKSeconds = (n: int, k: int) => {
  let rec loop = (child: int, direction: direction, count: int) => {
    switch count === k {
    | true => child < 0 ? 1 : child
    | false =>
      switch direction {
      | Left =>
        switch child < 0 {
        | true => loop(0, Right, count)
        | false => loop(child - 1, Left, count + 1)
        }
      | Right =>
        switch child === n {
        | true => loop(child - 2, Left, count)
        | false => loop(child + 1, Right, count + 1)
        }
      }
    }
  }

  loop(0, Right, 0)
}

let n1 = 3
let k1 = 5
let r1 = findChildWithBallAfterKSeconds(n1, k1)
Console.log2("r1: ", r1) // 1

let n2 = 5
let k2 = 6
let r2 = findChildWithBallAfterKSeconds(n2, k2)
Console.log2("r2: ", r2) // 2

let n3 = 4
let k3 = 2
let r3 = findChildWithBallAfterKSeconds(n3, k3)
Console.log2("r3: ", r3) // 2
