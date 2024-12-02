// INCORRECT - result set logic

let convertArrayInto2DArrayWithConditions = (nums: array<int>) => {
  let length = Array.length(nums)

  let rec makeFreqTable = (freqTable: Map.t<int, int>, index: int) => {
    switch index === length {
    | true => freqTable
    | false => {
        let num = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }
        let existingCount = switch freqTable->Map.get(num) {
        | None => 0
        | Some(c) => c
        }
        freqTable->Map.set(num, existingCount + 1)

        makeFreqTable(freqTable, index + 1)
      }
    }
  }

  let freqTable = makeFreqTable(Map.make(), 0)

  Console.log2("freqTable: ", freqTable)

  let rowsRequired = switch freqTable
  ->Map.values
  ->Array.fromIterator
  ->Array.toSorted((n1, n2) => Int.compare(n2, n1))
  ->Array.at(0) {
  | None => 0
  | Some(r) => r
  }

  Console.log2("rowsRequired: ", rowsRequired)

  let rec addNumToResultSet = (resultSet: array<Set.t<int>>, num: int, index: int) => {
    switch index === Array.length(resultSet) {
    | true => resultSet
    | false => {
        let set = switch resultSet->Array.at(index) {
        | None => Set.make()
        | Some(s) => s
        }

        switch set->Set.has(num) {
        | true => addNumToResultSet(resultSet, num, index + 1)
        | false => {
            set->Set.add(num)
            addNumToResultSet(resultSet, num, index + 1)
          }
        }
      }
    }
  }

  let rec convert = (resultSet: array<Set.t<int>>, index: int) => {
    switch index === length {
    | true => resultSet->Array.map(set => set->Set.values->Array.fromIterator)
    | false => {
        let num = switch nums->Array.at(index) {
        | None => 0
        | Some(n) => n
        }

        let newResultSet = addNumToResultSet(resultSet, num, 0)

        Console.log("\n")
        Console.log2("index: ", index)
        Console.log2("num: ", num)
        Console.log2("newResultSet: ", newResultSet)

        convert(newResultSet, index + 1)
      }
    }
  }

  let resultSet = Array.make(~length=rowsRequired, Set.make())
  Console.log2("resultSet: ", resultSet)
  convert(resultSet, 0)
}

let n1 = [1, 3, 4, 1, 2, 3, 1]
let r1 = convertArrayInto2DArrayWithConditions(n1)
Console.log2("r1: ", r1) // [[1, 3, 4, 2], [1, 3], [1]]
