// T(n) = O(n * m * log(m)) where n is the length of the input array and m is the length of the longest word in the array
// S(n) = O(n)

let groupAnagrams = (words: array<string>) => {
  let rec loop = (grouped: Map.t<string, array<string>>, index: int) => {
    switch index === Array.length(words) {
    | true => grouped
    | false => {
        let word = switch words->Array.at(index) {
        | None => ""
        | Some(c) => c
        }
        let sorted = String.split(word, "")->Array.toSorted(String.compare)->Array.join("")
        let existingGroups = switch grouped->Map.get(sorted) {
        | None => []
        | Some(g) => g
        }
        grouped->Map.set(sorted, existingGroups->Array.concat([word]))

        loop(grouped, index + 1)
      }
    }
  }

  loop(Map.make(), 0)->Map.values->Core__Iterator.toArray
}

let s1 = ["eat", "tea", "tan", "ate", "nat", "bat"]
let r1 = groupAnagrams(s1)
Console.log2("r1: ", r1)

let s2 = [""]
let r2 = groupAnagrams(s2)
Console.log2("r2: ", r2)

let s3 = ["a"]
let r3 = groupAnagrams(s3)
Console.log2("r3: ", r3)
