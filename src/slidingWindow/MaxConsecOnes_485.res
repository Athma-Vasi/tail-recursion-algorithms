// T(n) = O(n)
// S(n) = O(1)

let maxConsecutiveOnes = (binaryArray: array<int>) => {
  let length = Array.length(binaryArray)

  let rec loop = (~maxLength: int, ~counter: int, ~index: int) => {
    let binary = switch binaryArray->Array.get(index) {
    | None => 0
    | Some(b) => b
    }
    let newMaxLength = maxLength > counter ? maxLength : counter

    switch index === length {
    | true => newMaxLength
    | false =>
      switch binary === 0 {
      | true => loop(~maxLength=newMaxLength, ~counter=0, ~index=index + 1)
      | false => loop(~maxLength, ~counter=counter + 1, ~index=index + 1)
      }
    }
  }

  loop(~maxLength=Int32.min_int, ~counter=0, ~index=0)
}

let b1 = [1, 1, 0, 1, 1, 1]
let r1 = maxConsecutiveOnes(b1)
Console.log2("[1,1,0,1,1,1]", r1)

let b2 = [1, 0, 1, 1, 0]
let r2 = maxConsecutiveOnes(b2)
Console.log2("[1,0,1,1,0]", r2)
