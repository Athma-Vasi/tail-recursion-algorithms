// T(n) = O(n)
// S(n) = O(n)

let findTheEncryptedString = (str: string, k: int) => {
  let rec loop = (encrypted: string, index: int) => {
    let length = String.length(str)

    switch index === length {
    | true => encrypted
    | false => {
        let kthIndex = index + k
        let kthChar = str->String.charAt(kthIndex >= length ? kthIndex - (k + 1) : kthIndex)

        loop(encrypted->String.concat(kthChar), index + 1)
      }
    }
  }

  loop(String.make(), 0)
}

let s1 = "dart"
let k1 = 3
let r1 = findTheEncryptedString(s1, k1)
Console.log2("r1: ", r1) // tdar

let s2 = "aaa"
let k2 = 1
let r2 = findTheEncryptedString(s2, k2)
Console.log2("r2: ", r2) // aaa
