type rec t<'a> = {val: 'a, left: option<t<'a>>, right: option<t<'a>>}

let make = (~val: 'a, ~left=None, ~right=None): t<'a> => {
  {
    val,
    left,
    right,
  }
}
