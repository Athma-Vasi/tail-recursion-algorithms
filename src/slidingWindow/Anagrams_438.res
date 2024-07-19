let anagrams = (str: string, subString: string) => {
  let stringLength = String.length(str)

  let makeSubStringTable = (subString: string) =>
    String.split(subString, "")->Array.reduce(Map.make(), (acc, char) => {
      let existingCount = switch acc->Map.get(char) {
      | None => 0
      | Some(char) => char
      }
      acc->Map.set(char, existingCount + 1)
      acc
    })

  // expand window to subString length
  let rec expandWindow = (
    subStringTable: Map.t<string, int>,
    ~lowIndex: int,
    ~newLowIndex: int,
    ~highIndex: int,
  ) => {
    switch highIndex - lowIndex + 1 === Map.size(subStringTable) {
    | true => (subStringTable, highIndex)
    | false => {
        let highChar = switch str->String.get(newLowIndex) {
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

        expandWindow(subStringTable, ~lowIndex, ~newLowIndex, ~highIndex=highIndex + 1)
      }
    }
  }

  let rec loop = (
    startIndices: array<int>,
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
      // if valid anagram exists at max right window
      | true => startIndices->Array.concat([lowIndex])
      | false => startIndices
      }
    | false =>
      switch lowIndex === highIndex {
      | true => loop(startIndices, subStringTable, lowIndex, highIndex + 1)
      | false =>
        switch counts === 0 {
        // if not at end of string and anagram is found
        | true => {
            // reset the count table
            let initialSubStringTable = makeSubStringTable(subString)

            let newLowIndex = highIndex + 1
            let lowChar = switch str->String.get(newLowIndex) {
            | None => ""
            | Some(char) => char
            }
            switch initialSubStringTable->Map.has(lowChar) {
            | true => {
                let existingCount = switch initialSubStringTable->Map.get(lowChar) {
                | None => 0
                | Some(count) => count
                }
                initialSubStringTable->Map.set(lowChar, existingCount - 1)
              }
            | false => ()
            }

            let (newSubStringTable, newHighIndex) = expandWindow(
              initialSubStringTable,
              ~lowIndex,
              ~newLowIndex,
              ~highIndex=highIndex + 2,
            )

            loop(
              startIndices->Array.concat([lowIndex]),
              newSubStringTable,
              newLowIndex,
              newHighIndex,
            )
          }
        | false => {
            // if not at end of string and anagram not found
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
                // increment count when left char moves out of window
                subStringTable->Map.set(lowChar, existingCount + 1)
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
                // decrement count when right char moves into window
                subStringTable->Map.set(highChar, existingCount - 1)
              }
            | false => ()
            }

            loop(startIndices, subStringTable, lowIndex + 1, highIndex + 1)
          }
        }
      }
    }
  }

  let subStringTable = makeSubStringTable(subString)

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
      // increment count when left char moves out of window
      subStringTable->Map.set(firstChar, existingCount + 1)
    }
  | false => ()
  }

  let (newSubStringTable, newHighIndex) = expandWindow(
    subStringTable,
    ~lowIndex=0,
    ~newLowIndex=1,
    ~highIndex=1,
  )

  loop([], newSubStringTable, 0, newHighIndex)
}

let s1 = "cbaebabacd"
let p1 = "abc"
let r1 = anagrams(s1, p1)
Console.log2("cbaebabacd   abc", r1)
