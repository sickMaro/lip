type ast =
    Const of int
  | Add of ast * ast
  | Minus of ast * ast
  | Div of ast * ast
  | Mol of ast * ast
  | Um of ast
