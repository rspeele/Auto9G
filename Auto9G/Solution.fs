namespace Formulas.Internals
open System

module private FractionUtilities =
    let rec greatestCommonDivisor a b =
        if b = 0L then a
        else
            greatestCommonDivisor b (a % b)

    let commonDenominator a b =
        a * b / greatestCommonDivisor a b
open FractionUtilities

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
            | Literal x, Literal y -> Literal (x * y)
            | x, (Divide (n, d))
            | (Divide (n, d)), x when d = x -> n
            | SquareRoot x, SquareRoot y when x = y -> x
            | l, r -> Multiply(l, r)
        | Divide (l, r) ->
            match l.Simplify(), r.Simplify() with
            | (Literal 0L as zero, _) -> zero
            | x, Literal 1L -> x
            | x, y when x = y -> Literal 1L
            | Literal x, Literal y when y <> 0L && x % y = 0L -> Literal (x / y)
            | x, Multiply(Literal n, y) when x = y -> Literal 1L / Literal n
            | Multiply(Literal n, y), x when x = y -> Literal n
            | l, r -> Divide(l, r)
        | Add (l, r) ->
            match l.Simplify(), r.Simplify() with
            | Literal 0L, x
            | x, Literal 0L -> x
            | Literal x, Literal y -> Literal (x + y)
            | x, y when x = y -> Multiply(Literal 2L, x)
            | Divide(Literal ln, Literal ld), Divide(Literal rn, Literal rd) when rd <> 0L && ld <> 0L ->
                let denominator = commonDenominator ld rd
                let numerator = denominator / ld * ln + denominator / rd * rn
                (Divide(Literal numerator, Literal denominator)).Simplify()
            | l, r -> Add (l, r)
        | SquareRoot x ->
            match x.Simplify() with
            | Multiply(x1, x2) when x1 = x2 -> x1
            | x -> SquareRoot x
        | CubeRoot x ->
            match x.Simplify() with
            | Multiply(x1, Multiply(x2, x3))
            | Multiply(Multiply(x1, x2), x3) when x1 = x2 && x2 = x3 -> x1
            | x -> CubeRoot x
        | Literal _
        | BoundVariable _ -> this
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