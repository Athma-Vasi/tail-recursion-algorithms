// T(n) = O(n)
// S(n) = O(n)

let groupPeopleGivenGroupSize = (groupSizes: array<int>) => {
  let rec groupPeople = (
    grouped: array<array<int>>,
    sizeTable: Map.t<int, array<int>>,
    index: int,
  ) => {
    switch index === Array.length(groupSizes) {
    | true => Map.values(sizeTable)->Array.fromIterator->Array.concat(grouped)
    | false => {
        let groupSize = switch groupSizes->Array.at(index) {
        | None => 0
        | Some(s) => s
        }
        let group = switch sizeTable->Map.get(groupSize) {
        | None => []
        | Some(g) => g
        }

        switch Array.length(group) >= 3 {
        | true => {
            sizeTable->Map.set(groupSize, [index])
            groupPeople(grouped->Array.concat([group]), sizeTable, index + 1)
          }
        | false => {
            sizeTable->Map.set(groupSize, group->Array.concat([index]))
            groupPeople(grouped, sizeTable, index + 1)
          }
        }
      }
    }
  }

  groupPeople([], Map.make(), 0)
}

let g1 = [3, 3, 3, 3, 3, 1, 3]
let r1 = groupPeopleGivenGroupSize(g1)
Console.log2("r1: ", r1) // [[0, 1, 2], [3, 4, 6], [5]]

let g2 = [2, 1, 3, 3, 3, 2]
let r2 = groupPeopleGivenGroupSize(g2)
Console.log2("r2: ", r2) // [[1], [0, 5], [2, 3, 4]]
