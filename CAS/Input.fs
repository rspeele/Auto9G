namespace Formulas

type InputExpr =
    | InputVar of Variable
    | InputNum of Solution
    | InputNeg of InputExpr
    | InputMul of InputExpr * InputExpr
    | InputDiv of InputExpr * InputExpr
    | InputAdd of InputExpr * InputExpr
    | InputSub of InputExpr * InputExpr
    override this.ToString() =
        match this with
        | InputVar v -> string v
        | InputNum n -> string n
        | InputNeg e -> "-" + string e
        | InputMul (x, y) -> "(" + string x + " * " + string y + ")"
        | InputDiv (x, y) -> "(" + string x + " / " + string y + ")"
        | InputAdd (x, y) -> "(" + string x + " + " + string y + ")"
        | InputSub (x, y) -> "(" + string x + " - " + string y + ")"

type Equation =
    | Equation of InputExpr * InputExpr