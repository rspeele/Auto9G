module CAS.Main
open System

let rec greatestCommonDivisor a b =
    if b = 0L then a
    else
        greatestCommonDivisor b (a % b)

let commonDenominator a b =
    a * b / greatestCommonDivisor a b

type Variable =
    | Variable of string
    override this.ToString() = this.Name
    member this.Name = let (Variable x) = this in x

type Solution =
    | Literal of int64
    | BoundVariable of Variable
    | Multiply of Solution * Solution
    | Divide of Solution * Solution
    | Add of Solution * Solution
    | SquareRoot of Solution
    | CubeRoot of Solution
    static member Zero =
        Literal 0L
    static member Sqrt(x) =
        (SquareRoot x).Simplify()
    static member ( * ) (l, r) =
        (Multiply (l, r)).Simplify()
    static member ( / ) (l, r) =
        (Divide (l, r)).Simplify()
    static member ( ~- ) (x) =
        (Multiply(Literal -1L, x)).Simplify()
    static member ( + ) (l, r) =
        (Add (l, r)).Simplify()
    static member ( - ) (l, r) =
        (Add (l, -r)).Simplify()
    member this.Simplify() =
        match this with
        | Multiply (l, r) ->
            match l.Simplify(), r.Simplify() with
            | (Literal 0L as zero, _)
            | (_, (Literal 0L as zero)) -> zero
            | Literal 1L, x
            | x, Literal 1L -> x
            | x, (Divide (n, d))
            | (Divide (n, d)), x when d = x -> n
            | SquareRoot x, SquareRoot y when x = y -> x
            | l, r -> Multiply(l, r)
        | Divide (l, r) ->
            match l.Simplify(), r.Simplify() with
            | (Literal 0L as zero, _) -> zero
            | x, Literal 1L -> x
            | x, y when x = y -> Literal 1L
            | x, Multiply(Literal n, y) when x = y -> Literal 1L / Literal n
            | Multiply(Literal n, y), x when x = y -> Literal n
            | l, r -> Divide(l, r)
        | Add (l, r) ->
            match l.Simplify(), r.Simplify() with
            | Literal 0L, x
            | x, Literal 0L -> x
            | Literal x, Literal y -> Literal (x + y)
            | x, y when x = y -> Multiply(Literal 2L, x)
            | l, r -> Add (l, r)
        | other -> other
    member this.Approximate(variables : Variable -> decimal) =
        match this with
        | Literal x -> decimal x
        | BoundVariable v -> variables v
        | Multiply (x, y) ->
            x.Approximate(variables) * y.Approximate(variables)
        | Divide (x, y) ->
            x.Approximate(variables) / y.Approximate(variables)
        | Add (x, y) ->
            x.Approximate(variables) + y.Approximate(variables)
        | SquareRoot x ->
            decimal (sqrt (double (x.Approximate(variables))))
        | CubeRoot x ->
            decimal (Math.Pow(double (x.Approximate(variables)), 1.0/3.0))
    override this.ToString() =
        match this with
        | Literal x -> string x
        | BoundVariable v -> string v
        | Multiply (x, y) -> sprintf "(%O*%O)" x y
        | Divide (x, y) -> sprintf "(%O/%O)" x y
        | Add (x, y) -> sprintf "(%O+%O)" x y
        | SquareRoot x -> sprintf "sqrt(%O)" x
        | CubeRoot x -> sprintf "(%O^1/3)" x

module NumericLiteralN =
    let FromOne () = Literal 1L
    let FromZero () = Literal 0L
    let FromInt32 i = Literal (int64 i)
    let FromInt64 i = Literal i
    let FromString (s : string) = Literal (int64 s)
    
type InputExpr =
    | InputVar of Variable
    | InputNum of Solution
    | InputNeg of InputExpr
    | InputMul of InputExpr * InputExpr
    | InputDiv of InputExpr * InputExpr
    | InputAdd of InputExpr * InputExpr
    | InputSub of InputExpr * InputExpr

type Power = int

type PolyTerm =
    | PolyTerm of Solution * Power
    member this.Coefficient = let (PolyTerm(c, _)) = this in c
    member this.Power = let (PolyTerm(_, p)) = this in p
    static member Zero =
        PolyTerm (0N, 0)
    static member One =
        PolyTerm (1N, 0)
    static member ( * ) (PolyTerm(ls, lp), PolyTerm(rs, rp)) =
        PolyTerm(ls * rs, lp + rp)
    static member ( / ) (PolyTerm(ls, lp), PolyTerm(rs, rp)) =
        PolyTerm(ls / rs, lp - rp)
    static member ( ~- ) (PolyTerm(s, p)) =
        PolyTerm(-s, p)

let power (PolyTerm (_, p)) = p
let coeff (PolyTerm (c, _)) = c

type Poly =
    | Poly of PolyTerm list
    member this.Terms = let (Poly terms) = this in terms
    member private this.CombineLikeTerms() =
        let likeTerms =
            this.Terms
            |> Seq.groupBy power
            |> Seq.map (fun (power, terms) ->
                PolyTerm (Seq.sumBy coeff terms, power))
            |> Seq.sortByDescending power
            |> Seq.toList
        Poly likeTerms
    override this.ToString() =
        let (Poly terms) = this.CombineLikeTerms()
        seq {
            for PolyTerm(n, power) in terms ->
                if power = 0 then string n
                elif power = 1 then string n + "x"
                else string n + "x^" + string power
        } |> String.concat " + "
    static member ( ~- ) (Poly terms) =
        Poly [ for t in terms -> -t ]
    static member ( * ) (Poly leftTerms, Poly rightTerms) =
        seq {
            for leftTerm in leftTerms ->
                [ for rightTerm in rightTerms ->
                    leftTerm * rightTerm
                ] |> Poly
        } |> Seq.fold (+) (Poly [])
    static member ( / ) (Poly leftTerms, Poly rightTerms)  =
        seq {
            for leftTerm in leftTerms ->
                [ for rightTerm in rightTerms ->
                    leftTerm / rightTerm
                ] |> Poly
        } |> Seq.fold (+) (Poly [])
    static member ( + ) (Poly left, Poly right) =
        (Poly (List.append left right)).CombineLikeTerms()
    static member ( - ) (left : Poly, right : Poly) =
        left + -right

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
    | _ -> failwith "only up to quadratic implemented so far"

type Equation =
    | Equation of InputExpr * InputExpr

let solveEquation solvingForVar (Equation(left, right)) =
    let left = toPolynomial solvingForVar left
    let right = toPolynomial solvingForVar right
    let zero = right - left
    let quartic = toQuartic zero
    zeroes quartic

[<EntryPoint>]
let main argv =
    let v name = InputVar (Variable name)
    // price = cost + (cost * markup)
    let equation = Equation(v "price", InputAdd(v "cost", InputMul(v "cost", v "markup")))

    let freeVariable = Variable "markup"
    let values =
        [   Variable "cost", 10m
            Variable "markup", 0.5m
            Variable "price", 15m
        ] |> Map.ofList

    let solutions = solveEquation freeVariable equation

    match solutions with
    | [] -> printfn "no solutions!"
    | solutions ->
        for x in solutions do
            printfn "%O = %O" freeVariable x
            let answer = x.Approximate(fun v -> Map.find v values)
            printfn "%O = %O" freeVariable answer

    0 // return an integer exit code
