// T(n) = O(n)
// S(n) = O(1)

let lemonadeChange = (bills: array<int>) => {
  let rec loop = (register: Map.t<int, int>, index: int) => {
    switch index === Array.length(bills) {
    | true => true
    | false => {
        let bill = switch bills->Array.at(index) {
        | None => 0
        | Some(b) => b
        }

        let fivesCount = switch register->Map.get(5) {
        | None => 0
        | Some(c) => c
        }
        let tensCount = switch register->Map.get(10) {
        | None => 0
        | Some(c) => c
        }
        let twentiesCount = switch register->Map.get(20) {
        | None => 0
        | Some(c) => c
        }

        switch bill === 0 {
        | true => false
        | false =>
          switch bill === 5 {
          | true => {
              register->Map.set(bill, fivesCount + 1)

              loop(register, index + 1)
            }
          | false => {
              let change = bill - 5

              switch change === 5 {
              | true =>
                switch fivesCount === 0 {
                | true => false
                | false => {
                    register->Map.set(5, fivesCount - 1)
                    register->Map.set(10, tensCount + 1)

                    loop(register, index + 1)
                  }
                }
              | false =>
                switch change !== 15 {
                | true => false
                | false =>
                  switch tensCount === 0 || fivesCount === 0 {
                  | true => false
                  | false => {
                      register->Map.set(5, fivesCount - 1)
                      register->Map.set(10, tensCount - 1)
                      register->Map.set(20, twentiesCount + 1)

                      loop(register, index + 1)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  loop(Map.make(), 0)
}

let b1 = [5, 5, 5, 10, 20]
let r1 = lemonadeChange(b1)
Console.log2("r1: ", r1) // true

let b2 = [5, 5, 10, 10, 20]
let r2 = lemonadeChange(b2)
Console.log2("r2: ", r2) // false
