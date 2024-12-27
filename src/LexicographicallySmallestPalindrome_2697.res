let lexicographicallySmallestPalindrome = (str: string) => {
  let rec operate = (palindrome: string, firstIndex: int, lastIndex: int) => {
    let length = String.length(palindrome)

    switch lastIndex - firstIndex < 2 {
    | true => palindrome
    | false => {
        let first = palindrome->String.charAt(firstIndex)
        let last = palindrome->String.charAt(lastIndex)

        switch first === last {
        | true => operate(palindrome, firstIndex + 1, lastIndex - 1)
        | false => {
            let firstCharCode = first->String.charCodeAt(0)
            let lastCharCode = last->String.charCodeAt(0)

            switch firstCharCode < lastCharCode {
            | true => {
                let newP =
                  palindrome
                  ->String.slice(~start=0, ~end=lastIndex)
                  ->String.concat(first)
                  ->String.sliceToEnd(~start=firstIndex + 1)

                operate(newP, firstIndex + 1, lastIndex - 1)
              }
            | false => {}
            }
          }
        }
      }
    }
  }
}
