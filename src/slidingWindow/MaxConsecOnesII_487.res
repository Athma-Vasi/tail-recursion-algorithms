// T(n) = O(n)
// S(n) = O(1)

let maxConsecutiveOnesII = (binaryArray: array<int>): int => {
  let length = Array.length(binaryArray)

  let rec loop = (~maxLength: int, ~counter: int, ~lowIndex: int, ~highIndex: int): int => {
    let highBinary = switch binaryArray->Array.get(highIndex) {
    | None => 0
    | Some(b) => b
    }
    let diff = highIndex - lowIndex
    let newMaxLength = maxLength > diff ? maxLength : diff

    switch highIndex === length {
    | true => newMaxLength
    | false =>
      switch highBinary === 0 {
      | true =>
        switch counter > 1 {
        | true => {
            // sliding left window to zero element
            let rec inner = (counter: int, lowIndexInner: int) => {
              let lowBinary = switch binaryArray->Array.get(lowIndexInner) {
              | None => 0
              | Some(b) => b
              }

              switch lowIndexInner === highIndex || lowBinary === 0 {
              | true => (counter, lowIndexInner)
              | false => inner(counter - 1, lowIndexInner + 1)
              }
            }

            let (newCounter, newIndex) = inner(counter, lowIndex)
            loop(~maxLength, ~counter=newCounter, ~lowIndex=newIndex + 1, ~highIndex)
          }
        | false => loop(~maxLength, ~counter=counter + 1, ~lowIndex, ~highIndex=highIndex + 1)
        }
      | false => loop(~maxLength, ~counter=counter + 1, ~lowIndex, ~highIndex=highIndex + 1)
      }
    }
  }

  loop(~maxLength=Int32.min_int, ~counter=0, ~lowIndex=0, ~highIndex=1)
}

let n1 = [1, 0, 1, 10]
let r1 = maxConsecutiveOnesII(n1)
Console.log2("[1,0,1,10]", r1)
