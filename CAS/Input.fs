namespace Formulas

type InputExpr =
    | InputVar of Variable
    | InputNum of Solution
    | InputNeg of InputExpr
    | InputMul of InputExpr * InputExpr
    | InputDiv of InputExpr * InputExpr
    | InputAdd of InputExpr * InputExpr
    | InputSub of InputExpr * InputExpr

type Equation =
    | Equation of InputExpr * InputExpr