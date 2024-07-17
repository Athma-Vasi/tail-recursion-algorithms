// // T(n) = O(m * n) where m <= n
// S(n) = O(m)
// correct but abserdly ludacris

let minWindowSubstring = (str: string, subString: string) => {
  let stringLength = String.length(str)

  let rec loop = (
    result: string,
    subStringTable: Map.t<string, int>,
    lowIndex: int,
    highIndex: int,
  ) => {
    let counts =
      subStringTable
      ->Map.entries
      ->Array.fromIterator
      ->Array.reduce(0, (acc, (_char, count)) => acc + count)

    switch highIndex === stringLength {
    | true =>
      switch counts === 0 {
      // if valid solution exists at max right window
      | true => {
          let newResultLength = highIndex + 1 - lowIndex
          let prevResultLength = String.length(result)

          switch newResultLength < prevResultLength {
          | true => str->String.slice(~start=lowIndex, ~end=highIndex)
          | false => result
          }
        }
      | false => result
      }
    | false =>
      switch lowIndex === highIndex {
      | true => loop(result, subStringTable, lowIndex, highIndex + 1)
      | false =>
        switch counts === 0 {
        // if not at end of string and all chars in subString are found
        | true => {
            // reset the count table
            let initialSubStringTable = String.split(subString, "")->Array.reduce(Map.make(), (
              acc,
              char,
            ) => {
              let existingCount = switch acc->Map.get(char) {
              | None => 0
              | Some(char) => char
              }
              acc->Map.set(char, existingCount + 1)
              acc
            })

            // if new low and high char exist, decrement count in table

            let newLowIndex = highIndex
            let newLowChar = switch str->String.get(newLowIndex) {
            | None => ""
            | Some(char) => char
            }
            switch subStringTable->Map.has(newLowChar) {
            | true => {
                let existingCount = switch subStringTable->Map.get(newLowChar) {
                | None => 0
                | Some(count) => count
                }
                subStringTable->Map.set(newLowChar, existingCount - 1)
              }
            | false => ()
            }

            let newHighIndex = highIndex + 1
            let newHighChar = switch str->String.get(newHighIndex) {
            | None => ""
            | Some(char) => char
            }
            switch subStringTable->Map.has(newHighChar) {
            | true => {
                let existingCount = switch subStringTable->Map.get(newHighChar) {
                | None => 0
                | Some(count) => count
                }
                subStringTable->Map.set(newHighChar, existingCount - 1)
              }
            | false => ()
            }

            let newResultLength = newLowIndex - lowIndex
            let prevResultLength = String.length(result)

            // set result to smaller and continue with new low and high pointers
            switch newResultLength < prevResultLength {
            | true => {
                let newResult = str->String.slice(~start=lowIndex, ~end=highIndex)
                loop(newResult, initialSubStringTable, newLowIndex, newHighIndex)
              }
            | false => loop(result, initialSubStringTable, newLowIndex, newHighIndex)
            }
          }
        | false =>
          // if not at end of string and not all chars in subString are found

          switch counts === String.length(subString) {
          // if no chars in subString are found
          | true => {
              // increment both pointers and decrement count if chars exist

              let newLowChar = switch str->String.get(lowIndex + 1) {
              | None => ""
              | Some(char) => char
              }
              switch subStringTable->Map.has(newLowChar) {
              | true => {
                  let existingCount = switch subStringTable->Map.get(newLowChar) {
                  | None => 0
                  | Some(count) => count
                  }
                  subStringTable->Map.set(newLowChar, existingCount - 1)
                }
              | false => ()
              }

              let newHighIndex = highIndex + 1
              let newHighChar = switch str->String.get(newHighIndex) {
              | None => ""
              | Some(char) => char
              }

              // if right window char exists, it is the new low index
              let newLowIndex = switch subStringTable->Map.has(newHighChar) {
              | true => {
                  let existingCount = switch subStringTable->Map.get(newHighChar) {
                  | None => 0
                  | Some(count) => count
                  }
                  subStringTable->Map.set(newHighChar, existingCount - 1)
                  newHighIndex
                }
              | false => lowIndex + 1
              }

              loop(result, subStringTable, newLowIndex, newHighIndex)
            }
          | false => {
              // left char is in subString

              let highChar = switch str->String.get(highIndex) {
              | None => ""
              | Some(char) => char
              }
              // if right window char exists, decrement count in table
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

              // increase right window
              loop(result, subStringTable, lowIndex, highIndex + 1)
            }
          }
        }
      }
    }
  }

  // initialize table and first,second chars and decrement counts

  let subStringTable = String.split(subString, "")->Array.reduce(Map.make(), (acc, char) => {
    let existingCount = switch acc->Map.get(char) {
    | None => 0
    | Some(char) => char
    }
    acc->Map.set(char, existingCount + 1)
    acc
  })

  let firstChar = switch str->String.get(0) {
  | None => ""
  | Some(char) => char
  }
  switch subStringTable->Map.has(firstChar) {
  | true => {
      let existingCount = switch subStringTable->Map.get(firstChar) {
      | None => 0
      | Some(count) => count
      }
      subStringTable->Map.set(firstChar, existingCount - 1)
    }
  | false => ()
  }

  let secondChar = switch str->String.get(1) {
  | None => ""
  | Some(char) => char
  }
  switch subStringTable->Map.has(secondChar) {
  | true => {
      let existingCount = switch subStringTable->Map.get(secondChar) {
      | None => 0
      | Some(count) => count
      }
      subStringTable->Map.set(secondChar, existingCount - 1)
    }
  | false => ()
  }

  loop(str ++ subString, subStringTable, 0, 2) // (arbitrary) initial result length must be greater than str length
}

let s1 = "ADOBECODEBANC"
let t1 = "ABC"
let r1 = minWindowSubstring(s1, t1)
Console.log2("ADOBECODEBANC ABC", r1)
