type~ g2 = #g2 ([]i32) | #nog

let foo2 (x: g2): g2 =
  match x
  case #nog -> #g2 []
  case _ -> x
