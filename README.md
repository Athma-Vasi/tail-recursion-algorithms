# Project Overview

This repository archives my solutions to various algorithmic problems from LeetCode.

- **Language** - Solutions are implemented in ReScript, a robust, type-safe language that compiles to highly performant JavaScript.
- **Style/Optimization Focus** - A core constraint for every solution was implementing it in a *tail-recursive* style, ensuring stack-safe and memory-efficient computation by converting recursion into an iterative loop during compilation.
- **Goal** - To practice and demonstrate proficiency in both *functional programming* paradigms and *algorithmic problem-solving*.


## Example Solution 

To showcase the tail-recursive approach, here is a simplified example:

```
// Example: Summing a list of integers using tail recursion

let listSum = (list: list<int>): int => {
  let rec sumHelper = (list, accumulator) =>
    switch list {
    | list{} => accumulator
    | list{head, ...tail} => sumHelper(tail, accumulator + head)
    }

  sumHelper(list, 0)
}

/* This implementation ensures that the recursive call (sumHelper(tail, accumulator + head)) 
is the last operation performed in the function, allowing the ReScript compiler 
to optimize it into a fast, iterative loop.
*/
```

## Key Takeaways

This repository highlights the following skills:

**ReScript/Functional Programming**: Experience with a modern, strongly-typed functional language.

**Tail Recursion & Optimization**: Practical application of advanced optimization techniques for stack-safe and efficient code.

**Algorithmic Proficiency**: Competence in solving a wide range of common data structure and algorithm problems.

**Code Quality & Consistency**: Adherence to consistent file naming, code style, and documentation.


