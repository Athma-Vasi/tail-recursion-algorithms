// T(n) = O(n)
// S(n) = O(1)

let optimalPartitionOfString = (str: string) => {
  let rec partition = (partitioned: array<array<string>>, substring: Set.t<string>, index: int) => {
    switch index > String.length(str) {
    | true => partitioned->Array.concat([substring->Set.values->Array.fromIterator])->Array.length
    | false => {
        let char = str->String.charAt(index)

        switch substring->Set.has(char) {
        | true =>
          partition(
            partitioned->Array.concat([substring->Set.values->Array.fromIterator]),
            Set.make(),
            index,
          )
        | false => {
            substring->Set.add(char)
            partition(partitioned, substring, index + 1)
          }
        }
      }
    }
  }

  partition([], Set.make(), 0)
}

let s1 = "abacaba"
let r1 = optimalPartitionOfString(s1)
Console.log2("r1: ", r1) // 4

let s2 = "ssssss"
let r2 = optimalPartitionOfString(s2)
Console.log2("r2: ", r2) // 6
