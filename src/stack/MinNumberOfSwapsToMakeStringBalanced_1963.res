// T(n) = O(n)
// S(n) = O(n)

let minNumberOfSwapsToMakeStringBalanced = (str: string) => {
  let rec swap = (count: int, stack: string, index: int) => {
    let stackLen = String.length(stack)

    switch index === String.length(str) {
    | true => (count + 1) / 2
    | false => {
        let curr = str->String.charAt(index)
        let top = stack->String.charAt(stackLen - 1)

        switch curr {
        | "]" =>
          switch top {
          | "[" => swap(count, stack->String.slice(~start=0, ~end=stackLen - 1), index + 1)
          // empty stack
          | _ => swap(count + 1, stack, index + 1)
          }
        // "["
        | _ => swap(count, stack->String.concat(curr), index + 1)
        }
      }
    }
  }

  swap(0, String.make(), 0)
}

let s1 = "][]["
let r1 = minNumberOfSwapsToMakeStringBalanced(s1)
Console.log2("r1: ", r1) // 1

let s2 = "]]][[["
let r2 = minNumberOfSwapsToMakeStringBalanced(s2)
Console.log2("r2: ", r2) // 2

let s3 = "[]"
let r3 = minNumberOfSwapsToMakeStringBalanced(s3)
Console.log2("r3: ", r3) // 0
