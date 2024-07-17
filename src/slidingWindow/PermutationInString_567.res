// T(n) = O(m * n) where m <= n
// S(n) = O(m)

let permutationInString = (subString: string, str: string) => {
  let stringLength = String.length(str)

  let subStringTable = String.split(subString, "")->Array.reduce(Map.make(), (acc, char) => {
    let existingCount = switch acc->Map.get(char) {
    | None => 0
    | Some(char) => char
    }
    acc->Map.set(char, existingCount + 1)
    acc
  })

  let rec loop = (subStrTable: Map.t<string, int>, lowIndex: int, highIndex: int) => {
    let counts =
      subStringTable
      ->Map.entries
      ->Array.fromIterator
      ->Array.reduce(0, (acc, (_char, count)) => acc + count)

    switch highIndex === stringLength {
    | true => counts === 0
    | false =>
      switch counts === 0 {
      | true => true
      | false => {
          let lowChar = switch str->String.get(lowIndex) {
          | None => ""
          | Some(char) => char
          }
          switch subStringTable->Map.has(lowChar) {
          | true => {
              let existingCount = switch subStringTable->Map.get(lowChar) {
              | None => 0
              | Some(count) => count
              }
              subStringTable->Map.set(lowChar, existingCount - 1)
            }
          | false => ()
          }

          let highChar = switch str->String.get(highIndex) {
          | None => ""
          | Some(char) => char
          }
          switch subStringTable->Map.has(highChar) {
          | true => {
              let existingCount = switch subStringTable->Map.get(highChar) {
              | None => 0
              | Some(count) => count
              }
              subStringTable->Map.set(highChar, existingCount - 1)
            }
          | false => ()
          }

          // keep window size fixed and continue to slide
          loop(subStrTable, lowIndex + 1, highIndex + 1)
        }
      }
    }
  }

  // sets the initial window size and subStrTable counts
  let rec initializeLoop = (subStringTable: Map.t<string, int>, highIndex: int) => {
    switch highIndex === Map.size(subStringTable) {
    | true => (subStringTable, highIndex)
    | false => {
        let highChar = switch str->String.get(highIndex) {
        | None => ""
        | Some(char) => char
        }

        switch subStringTable->Map.has(highChar) {
        | true => {
            let existingCount = switch subStringTable->Map.get(highChar) {
            | None => 0
            | Some(count) => count
            }
            subStringTable->Map.set(highChar, existingCount - 1)
          }
        | false => ()
        }

        initializeLoop(subStringTable, highIndex + 1)
      }
    }
  }

  let (newSubStringTable, newHighIndex) = initializeLoop(subStringTable, 1)
  loop(newSubStringTable, 0, newHighIndex)
}

let s1 = "ab"
let s11 = "eidbaooo"
let r1 = permutationInString(s1, s11)
Console.log2("ab eidbaooo", r1)

let s2 = "ab"
let s22 = "eidboaoo"
let r2 = permutationInString(s2, s22)
Console.log2("ab eidboaoo", r2)
