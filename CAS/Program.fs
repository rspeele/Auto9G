module CAS.Main

let rec gcd a b =
    if b = 0L then a
    else
        gcd b (a % b)

type Variable =
    | Variable of string

type Number =
    | Fraction of numerator : int64 * denominator : int64
    override this.ToString() =
        let (Fraction (n, d)) = this
        if d = 1L then string n
        else string n + "/" + string d
    static member ( * ) (Fraction (ln, ld), Fraction (rn, rd)) =
        let n = ln * rn
        let d = ld * rd
        let gcd = gcd n d
        Fraction (n / gcd, d / gcd)
    static member ( / ) (left : Number, Fraction (rn, rd)) =
        left * Fraction(rd, rn)
    static member ( + ) (Fraction (ln, ld), Fraction (rn, rd)) =
        if ld = rd then Fraction(ln + rn, ld)
        else
            let d =
                ld * rd / gcd ld rd
            let n =
                ln * (d / ld) + rn * (d / rd)
            Fraction (n, d)
    static member ( - ) (left : Number, right : Number) =
        left + -right
    static member ( ~- ) (Fraction (rn, rd)) =
        Fraction (-rn, rd)

module NumericLiteralN =
    let FromOne () = Fraction(1L, 1L)
    let FromZero () = Fraction(0L, 1L)
    let FromInt32 i = Fraction(int64 i, 1L)
    let FromInt64 i = Fraction(i, 1L)
    let FromString s = Fraction(int64 s, 1L)
    
type IExpr = // input expr AST
    | IVar of Variable
    | INum of Number
    | INeg of IExpr
    | IMul of IExpr * IExpr
    | IDiv of IExpr * IExpr
    | IAdd of IExpr * IExpr
    | ISub of IExpr * IExpr

type PExpr = // single-variable form after plugging in known vars
    | PVar // the single variable we're solving for
    | PNum of Number
    | PNeg of PExpr
    | PMul of PExpr * PExpr
    | PDiv of PExpr * PExpr
    | PAdd of PExpr * PExpr

let rec constant e =
    match e with
    | PVar -> None
    | PNum n -> Some n
    | PNeg n -> Option.map (~-) (constant n)
    | PMul (l, r) ->
        match constant l, constant r with
        | Some l, Some r -> Some (l * r)
        | _ -> None
    | PDiv (l, r) ->
        match constant l, constant r with
        | Some l, Some r -> Some (l / r)
        | _ -> None
    | PAdd (l, r) ->
        match constant l, constant r with
        | Some l, Some r -> Some (l + r)
        | _ -> None

type Solution =
    | False
    | True
    | TrueWithVar of Number

type Polynomial =
    | Polynomial of Number list // [ 1; 2; 3 ] -> 3x^2 + 2x + 1
    override this.ToString() =
        let (Polynomial factors) = this
        seq {
            for power, n in Seq.indexed factors ->
                if power = 0 then string n
                elif power = 1 then string n + "x"
                else string n + "x^" + string power
        } |> String.concat " + "
    member private this.Multiply(n, power) =
        let (Polynomial xs) = this
        Polynomial
            [   for i = 1 to power do yield 0N
                for x in xs -> n * x
            ]
    static member Constant(n) = Polynomial [ n ]
    static member Linear(m, b) = Polynomial [ b; m ]
    static member Quadratic(a, b, c) = Polynomial [ c; b; a ]
    static member ( ~- ) (Polynomial factors) =
        Polynomial [ for f in factors -> -f ]
    static member ( * ) (Polynomial left, right : Polynomial) =
        seq {
            for power, n in Seq.indexed left ->
                right.Multiply(n, power)
        } |> Seq.fold (+) (Polynomial [])
    static member ( / ) (left : Polynomial, Polynomial right) =
        left * Polynomial [ for f in right -> 1N / f ]
    static member ( + ) (Polynomial left, Polynomial right) =
        match left, right with
        | [], [] -> Polynomial []
        | [], rs -> Polynomial rs
        | ls, [] -> Polynomial ls
        | (l :: ls), (r :: rs) ->
            let (Polynomial xs) = Polynomial ls + Polynomial rs
            Polynomial (l + r :: xs)
    static member ( - ) (left : Polynomial, right : Polynomial) =
        left + -right

module NumericLiteralP =
    let FromOne () = Polynomial.Constant(1N)
    let FromZero () = Polynomial.Constant(0N)
    let FromInt32 i = Polynomial.Constant(NumericLiteralN.FromInt32 i)
    let FromInt64 i = Polynomial.Constant(NumericLiteralN.FromInt64 i)
    let FromString s = Polynomial.Constant(NumericLiteralN.FromString s)

let rec toPolynomial e =
    match e with
    | PVar -> Polynomial.Linear(1N, 0N)
    | PNum n -> Polynomial.Constant(n)
    | PNeg n -> -(toPolynomial n)
    | PMul (l, r) -> toPolynomial l * toPolynomial r
    | PDiv (l, r) -> toPolynomial l / toPolynomial r
    | PAdd (l, r) -> toPolynomial l + toPolynomial r

//// price = cost + cost * markup

//// first refactor to put a constant on the left side

//// 1 = (cost + cost * markup) / price

//// next solve using the left side as the accumulator, till we end up with just a var on the right side
//let rec solve acc e =
//    match e with
//    | PVar -> [TrueWithVar acc]
//    | PNum n ->
//        if n = acc then [True] else [False]
//    | PNeg n ->
//        solve (-acc) e
//    | PMul (l, r) ->
//        // some special cases to consider
//        match constant l, constant r with
//        // 1. both sides evaulate to constants, in which case this is equivalent to a constant
//        | Some l, Some r ->
//            solve acc (PNum (l * r))
//        // 2. one side evaluates to a constant, in which case we can solve the other side
//        | None, Some r ->
//            solve (acc / r) l
//        | Some l, None ->
//            solve (acc / l) r
//        // 3. both sides contain a variable, in which case we have to get fancy
//        | None, None ->
//            failwith "need to convert this to a polynomial to solve"
//            // like n = ((x + 1) * x)
//            // becomes n = x^2 + x (by distributing)
//            // which we can solve w/ quadratic formula
//            // (or working backwards using +/- for the sqrts ourselves?)
//    | PDiv (l, r) ->
//        // some special cases to consider
//        match constant l, constant r with
//        // 1. both sides evaulate to constants, in which case this is equivalent to a constant
//        | Some l, Some r ->
//            solve acc (PNum (l / r))
//        // 2. one side evaluates to a constant, in which case we can solve the other side
//        | None, Some r ->
//            solve (acc * r) l
//        | Some l, None ->
//            solve (l / acc) r
//        | None, None ->
//            failwith "not sure what to do here, but probably starts by distributing"
//    | PAdd (l, r) ->
//        // some special cases to consider
//        match constant l, constant r with
//        // 1. both sides evaulate to constants, in which case this is equivalent to a constant
//        | Some l, Some r ->
//            solve acc (PNum (l + r))
//        // 2. one side evaluates to a constant, in which case we can solve the other side
//        | None, Some r ->
//            solve (acc - r) l
//        | Some l, None ->
//            solve (acc - l) r
//        | None, None ->
//            failwith "not sure what to do here, but probably starts by distributing"

[<EntryPoint>]
let main argv =
    let m = Polynomial [ 1N; 2N; 3N ]
    let n = Polynomial [ 1N; 2N; 3N; 4N ]
    printfn "%O" (m * n)
    printfn "%O" (n * m)
    0 // return an integer exit code
