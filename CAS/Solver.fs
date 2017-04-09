module Formulas.Internals.Solver

let rec toPolynomial solvingForVar e =
    match e with
    | InputVar v when v = solvingForVar -> Poly [ PolyTerm(1N, 1) ]
    | InputVar v -> Poly [ PolyTerm(BoundVariable v, 0) ]
    | InputNum n -> Poly [ PolyTerm(n, 0) ]
    | InputMul (l, r) -> toPolynomial solvingForVar l * toPolynomial solvingForVar r
    | InputDiv (l, r) -> toPolynomial solvingForVar l / toPolynomial solvingForVar r
    | InputAdd (l, r) -> toPolynomial solvingForVar l + toPolynomial solvingForVar r
    | InputSub (l, r) -> toPolynomial solvingForVar l - toPolynomial solvingForVar r
    | InputNeg e -> -toPolynomial solvingForVar e

type Quartic =
    {   X4 : Solution
        X3 : Solution
        X2 : Solution
        X1 : Solution
        X0 : Solution
        XN1 : Solution
    }
    static member Zero =
        {   X4 = 0N
            X3 = 0N
            X2 = 0N
            X1 = 0N
            X0 = 0N
            XN1 = 0N
        }

let toQuartic (Poly terms) =
    terms
    |> List.fold (fun acc (PolyTerm(c, p) as term) ->
            match p with
            | -1 -> { acc with XN1 = acc.XN1 + c }
            | 0 -> { acc with X0 = acc.X0 + c }
            | 1 -> { acc with X1 = acc.X1 + c }
            | 2 -> { acc with X2 = acc.X2 + c }
            | 3 -> { acc with X3 = acc.X3 + c }
            | 4 -> { acc with X4 = acc.X4 + c }
            | p ->
                failwithf "This polynomial includes the term x^%d. We can only handle up to quartic polynomials." p
        ) Quartic.Zero

let rec zeroes (q : Quartic) =
    match q with
    | { X4 = Literal 0L; X3 = Literal 0L; X2 = Literal 0L; X1 = m; X0 = b; XN1 = Literal 0L } ->
        [ -b / m ]
    | { X4 = Literal 0L; X3 = Literal 0L; X2 = Literal 0L; X1 = Literal 0L; X0 = b; XN1 = m } ->
        [ -b * m ]
    | { X4 = Literal 0L; X3 = Literal 0L; X2 = a; X1 = b; X0 = c; XN1 = Literal 0L } ->
        let square = b * b - 4N * a * c
        if square < 0N then []
        else
            let s = sqrt square
            [   (-b + s) / (2N * a)
                (-b - s) / (2N * a)
            ]
    | { X4 = Literal 0L; X3 = a; X2 = b; X1 = c; X0 = d; XN1 = Literal 0L } ->
        let square = b * b - 4N * a * c
        if square < 0N then []
        else
            let s = sqrt square
            [   (-b + s) / (2N * a)
                (-b - s) / (2N * a)
            ]
    | _ ->
        failwith "not implemented yet (cubic or quartic polynomial)"

let solveEquation solvingForVar (Equation(left, right)) =
    let left = toPolynomial solvingForVar left
    let right = toPolynomial solvingForVar right
    let zero = right - left
    let quartic = toQuartic zero
    zeroes quartic

