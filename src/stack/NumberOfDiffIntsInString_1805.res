let numberOfDiffIntsInString = (word: string) => {
  let rec loop = (intSet: Set.t<int>, stack: string, index: int) => {
    switch index === String.length(word) {
    | true => {
        let num = switch stack->Int.fromString {
        | None => Int32.min_int
        | Some(n) => n
        }
        switch num === Int32.min_int {
        | true => intSet->Set.size
        | false => {
            intSet->Set.add(num)
            intSet->Set.size
          }
        }
      }
    | false => {
        let currentChar = word->String.charAt(index)
        let currentDigit = switch Int.fromString(currentChar) {
        | None => -1
        | Some(dig) => dig
        }

        switch currentDigit < 0 {
        | true => {
            let num = switch stack->Int.fromString {
            | None => Int32.min_int
            | Some(n) => n
            }
            switch num === Int32.min_int {
            | true => loop(intSet, "", index + 1)
            | false => {
                intSet->Set.add(num)

                loop(intSet, "", index + 1)
              }
            }
          }
        | false => loop(intSet, stack->String.concat(currentChar), index + 1)
        }
      }
    }
  }

  loop(Set.make(), "", 0)
}

let w1 = "a123bc34d8ef34"
let r1 = numberOfDiffIntsInString(w1) // 3
Console.log2("r1: ", r1)

// let w2 = "leet1234code234"
// let r2 = numberOfDiffIntsInString(w2) // 2
// Console.log2("r2: ", r2)

// let w3 = "a1b01c001"
// let r3 = numberOfDiffIntsInString(w3) // 1
// Console.log2("r3: ", r3)
