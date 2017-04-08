module CAS.Main
open System

let rec gcd a b =
    if b = 0L then a
    else
        gcd b (a % b)

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
    
type IExpr = // input expr AST
    | IVar of Variable
    | INum of Solution
    | INeg of IExpr
    | IMul of IExpr * IExpr
    | IDiv of IExpr * IExpr
    | IAdd of IExpr * IExpr
    | ISub of IExpr * IExpr

type Power = int

type PolyTerm =
    | PolyTerm of Solution * Power
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
    member this.CombineLikeTerms() =
        let likeTerms =
            this.Terms
            |> Seq.append [ PolyTerm.Zero ]
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
    | IVar v when v = solvingForVar -> Poly [ PolyTerm(1N, 1) ]
    | IVar v -> Poly [ PolyTerm(BoundVariable v, 0) ]
    | INum n -> Poly [ PolyTerm(n, 0) ]
    | IMul (l, r) -> toPolynomial solvingForVar l * toPolynomial solvingForVar r
    | IDiv (l, r) -> toPolynomial solvingForVar l / toPolynomial solvingForVar r
    | IAdd (l, r) -> toPolynomial solvingForVar l + toPolynomial solvingForVar r
    | ISub (l, r) -> toPolynomial solvingForVar l - toPolynomial solvingForVar r
    | INeg e -> -toPolynomial solvingForVar e

let rec solutions (value : Solution) (p : Poly) =
    match p.CombineLikeTerms() with
    | Poly [ PolyTerm(k, 0) ] ->
        if value = k then [ None ] else []
    | Poly [ PolyTerm(k, 0); PolyTerm(m, -1) ] ->
        [ Some <| (value - k) * m ]
    | Poly [ PolyTerm(m, 1); PolyTerm(k, 0) ] ->
        let x = (value - k) / m
        [ Some x ]
    | Poly [ PolyTerm(a, 2); PolyTerm(b, 1); PolyTerm(c, 0) ] ->
        let square = b * b - 4N * a * c
        if square < 0N then []
        else
            let s = sqrt square
            [   Some ((-b + s) / (2N * a))
                Some ((-b - s) / (2N * a))
            ]
    | p -> failwithf "can't handle this fancy thing: %O" p

[<EntryPoint>]
let main argv =
    let v name = IVar (Variable name)
    let formula =
        // price = cost + cost * markup
        // 1 = (cost + cost * markup) / price
        IDiv(IAdd(v "cost", IMul(v "cost", v "markup")), v "price")

    let solveFor = Variable "cost"
    let poly = toPolynomial solveFor formula
    printfn "%O" poly

    let solutions =
        solutions 1N poly
    match solutions with
    | [] -> printfn "no solutions!"
    | solutions ->
        for solution in solutions do
            match solution with
            | None -> printfn "trivially true!"
            | Some x ->
                printfn "%O = %O" solveFor x

    0 // return an integer exit code
