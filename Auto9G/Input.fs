namespace Formulas.Internals

type InputExpr =
    | InputVar of Variable
    | InputNum of Solution
    | InputNeg of InputExpr
    | InputMul of InputExpr * InputExpr
    | InputDiv of InputExpr * InputExpr
    | InputAdd of InputExpr * InputExpr
    | InputSub of InputExpr * InputExpr
    member this.Variables() =
        match this with
        | InputVar v -> Seq.singleton v
        | InputNum _ -> Seq.empty
        | InputNeg e -> e.Variables()
        | InputMul (x, y) -> (x.Variables(), y.Variables()) ||> Seq.append
        | InputDiv (x, y) -> (x.Variables(), y.Variables()) ||> Seq.append
        | InputAdd (x, y) -> (x.Variables(), y.Variables()) ||> Seq.append
        | InputSub (x, y) -> (x.Variables(), y.Variables()) ||> Seq.append
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
    member this.Variables() =
        let (Equation (x, y)) = this
        (x.Variables(), y.Variables()) ||> Seq.append