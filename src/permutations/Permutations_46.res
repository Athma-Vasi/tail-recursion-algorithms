// INCORRECT - infinite loop

let permutations = (nums: array<int>) => {
  let rec loop = (
    result: array<array<int>>,
    current: array<int>,
    set: Set.t<int>,
    setIndex: int,
  ) => {
    switch Set.size(set) === 0 || setIndex >= Set.size(set) {
    | true => result->Array.concat([current])
    | false => {
        let val = switch set->Set.values->Array.fromIterator->Array.at(setIndex) {
        | None => Int32.min_int
        | Some(v) => v
        }
        let setCopy = Set.fromIterator(set->Set.values)
        setCopy->Set.delete(val)->ignore

        Console.log("\n")
        Console.log("--loop--")
        Console.log2("result: ", result)
        Console.log2("current: ", current)
        Console.log2("set: ", set)
        Console.log2("setIndex: ", setIndex)
        Console.log2("val: ", val)
        Console.log2("setCopy: ", setCopy)

        loop(result, current->Array.concat([val]), setCopy, setIndex + 1)
      }
    }
  }

  loop([], [], Set.fromArray(nums), 0)
}

let n1 = [1, 2, 3]
let r1 = permutations(n1)
Console.log2("r1: ", r1) // [ [ 1, 2, 3 ], [ 1, 3, 2 ], [ 2, 1, 3 ], [ 2, 3, 1 ], [ 3, 1, 2 ], [ 3, 2, 1 ] ]

let n2 = [0, 1]
let r2 = permutations(n2)
Console.log2("r2: ", r2) // [ [ 0, 1 ], [ 1, 0 ] ]

let n3 = [1]
let r3 = permutations(n3)
Console.log2("r3: ", r3) // [ [ 1 ] ]
