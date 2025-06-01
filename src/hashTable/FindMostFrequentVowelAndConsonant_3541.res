// T(n) = O(n)
// S(n) = O(n)

let findMostFrequentVowelAndConsonant = (s: string) => {
  let makeStringSet = (str: string) => {
    let rec loop = (set: Set.t<string>, index: int) => {
      switch index === String.length(str) {
      | true => set
      | false => {
          let char = str->String.charAt(index)
          set->Set.add(char)
          loop(set, index + 1)
        }
      }
    }

    loop(Set.make(), 0)
  }
  let vowelsSet = makeStringSet("aeiou")

  let splitVowelsAndConsonants = (str: string) => {
    let rec split = (vowels: string, consonants: string, index: int) => {
      switch index === String.length(str) {
      | true => (vowels, consonants)
      | false => {
          let char = str->String.charAt(index)
          let isVowel = vowelsSet->Set.has(char)

          isVowel
            ? split(vowels->String.concat(char), consonants, index + 1)
            : split(vowels, consonants->String.concat(char), index + 1)
        }
      }
    }

    split(String.make(), String.make(), 0)
  }
  let (vowels, consonants) = splitVowelsAndConsonants(s)

  let makeFreqTable = (str: string) => {
    let rec loop = (freqTable: Map.t<string, int>, index: int) => {
      switch index === String.length(str) {
      | true => freqTable
      | false => {
          let char = str->String.charAt(index)
          let freq = freqTable->Map.get(char)->Option.mapOr(1, freq => freq + 1)
          freqTable->Map.set(char, freq)

          loop(freqTable, index + 1)
        }
      }
    }

    loop(Map.make(), 0)
  }

  let findMaxFreq = (freqTable: Map.t<string, int>) =>
    freqTable
    ->Map.values
    ->Core__Iterator.toArray
    ->Array.toSorted((f1, f2) => Int.compare(f2, f1))
    ->Array.at(0)
    ->Option.mapOr(0, f => f)

  let vowelsFreqTable = makeFreqTable(vowels)
  let consonantsFreqTable = makeFreqTable(consonants)
  let maxVowelsFreq = findMaxFreq(vowelsFreqTable)
  let maxConsonantsFreq = findMaxFreq(consonantsFreqTable)

  maxVowelsFreq + maxConsonantsFreq
}

let s1 = "successes"
let r1 = findMostFrequentVowelAndConsonant(s1)
Console.log2("r1: ", r1) // 6

let s2 = "aeiaeia"
let r2 = findMostFrequentVowelAndConsonant(s2)
Console.log2("r2: ", r2)
