module CAS.Main
open System

let rec gcd a b =
    if b = 0L then a
    else
        gcd b (a % b)

type Variable =
    | Variable of string
    member this.Name =
        let (Variable x) = this in x

[<CustomEquality>]
[<CustomComparison>]
type Number =
    | Fraction of numerator : int64 * denominator : int64
    member inline private this.Numerator = let (Fraction(n, _)) = this in n
    member inline private this.Denominator = let (Fraction(_, d)) = this in d
    member this.Sign =
        let (Fraction (n, d)) = this
        if (n >= 0L) = (d >= 0L) then 1
        else -1
    override this.ToString() =
        let (Fraction (n, d)) = this
        if d = 1L then string n
        else string n + "/" + string d
    static member Zero = Fraction (0L, 1L)
    static member ( * ) (Fraction (ln, ld), Fraction (rn, rd)) =
        if ln = 0L || rn = 0L then Fraction(0L, 1L) else
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
    static member Abs(Fraction (rn, rd)) =
        Fraction (abs rn, abs rd)
    static member Sqrt (x : Number) =
        // TODO: represent irrationals deferring the sqrt...
        // this is just a placeholder to see if things are sort-of right
        let ratio = double x.Numerator / double x.Denominator
        let approx = sqrt ratio
        let scale = 1000L
        Fraction (int64 (approx * double scale), 1L) * Fraction(1L, scale)
    member this.CompareTo(other : Number) =
        if this.Sign > other.Sign then 1
        elif this.Sign < other.Sign then -1
        else
            let ratio = this / other
            if ratio.Numerator = ratio.Denominator then 0
            elif ratio.Numerator < ratio.Denominator then 1
            else -1
    member this.CompareTo(other : obj) =
        match other with
        | :? Number as other -> this.CompareTo(other)
        | _ -> -1
    member this.Equals(o : Number) =
        this.CompareTo(o) = 0
    override this.Equals(o : obj) =
        this.CompareTo(o) = 0
    override this.GetHashCode() =
        let (Fraction (n, d)) = this
        (n, d).GetHashCode()
    interface IComparable with
        member this.CompareTo(other) = this.CompareTo(other)
    interface IEquatable<Number> with
        member this.Equals(other) = this.Equals(other)
    interface IComparable<Number> with
        member this.CompareTo(other) = this.CompareTo(other)

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
    | PMul of PExpr * PExpr
    | PDiv of PExpr * PExpr
    | PAdd of PExpr * PExpr

let rec plugIn (vars : Map<Variable, Number option>) (ex : IExpr) =
    match ex with
    | IVar v ->
        match vars |> Map.tryFind v with
        | Some None -> PVar
        | Some (Some n) -> PNum n
        | None -> failwithf "Variable %O not bound to a value or for solving" v
    | INum n -> PNum n
    | INeg e -> PMul(PNum (-1N), plugIn vars e)
    | IMul (l, r) -> PMul(plugIn vars l, plugIn vars r)
    | IDiv (l, r) -> PDiv(plugIn vars l, plugIn vars r)
    | IAdd (l, r) -> PAdd(plugIn vars l, plugIn vars r)
    | ISub (l, r) -> PAdd(plugIn vars l, PMul(PNum (-1N), plugIn vars r))

type Solution =
    | False
    | True
    | TrueWithVar of Number

type Power = int

type Polynomial =
    | Polynomial of (Number * Power) list
    member private this.Multiply(n, power) =
        let (Polynomial factors) = this
        [ for (ln, lp) in factors ->
            ln * n, lp + power
        ] |> Polynomial
    member this.Canonical() =
        let (Polynomial factors) = this
        let noZeros =
            factors
            |> Seq.filter (fst >> ((<>) 0N))
            |> Seq.append [ (0N, 0) ]
        let likeTerms =
            noZeros
            |> Seq.groupBy snd
            |> Seq.map (fun (power, terms) ->
                Seq.sumBy fst terms, power)
            |> Seq.sortByDescending snd
            |> Seq.toList
        Polynomial likeTerms
    override this.ToString() =
        let (Polynomial factors) = this.Canonical()
        seq {
            for n, power in factors ->
                if power = 0 then string n
                elif power = 1 then string n + "x"
                else string n + "x^" + string power
        } |> String.concat " + "
    static member ( ~- ) (Polynomial factors) =
        Polynomial [ for f, p in factors -> -f, p ]
    static member ( * ) (Polynomial left, right : Polynomial) =
        seq {
            for n, power in left ->
                right.Multiply(n, power)
        } |> Seq.fold (+) (Polynomial [])
    static member ( / ) (left : Polynomial, Polynomial right) =
        left * Polynomial [ for f, p in right -> f, -p ]
    static member ( + ) (Polynomial left, Polynomial right) =
        (Polynomial (List.append left right)).Canonical()
    static member ( - ) (left : Polynomial, right : Polynomial) =
        left + -right

let rec toPolynomial e =
    match e with
    | PVar -> Polynomial [ (1N, 1) ]
    | PNum n -> Polynomial [ (n, 0) ]
    | PMul (l, r) -> toPolynomial l * toPolynomial r
    | PDiv (l, r) -> toPolynomial l / toPolynomial r
    | PAdd (l, r) -> toPolynomial l + toPolynomial r

let rec solutions (value : Number) (p : Polynomial) =
    match p.Canonical() with
    | Polynomial [ (k, 0) ] ->
        if value = k then [ None ] else []
    | Polynomial [ (k, 0); (m, -1) ] ->
        let x = (value - k) * m
        [ Some x ]
    | Polynomial [ (m, 1); (k, 0) ] ->
        let x = (value - k) / m
        [ Some x ]
    | Polynomial [ (a, 2); (b, 1); (c, 0) ] ->
        let square = b * b - 4N * a * c
        if square < 0N then []
        else
            let s = sqrt square
            [   Some ((-b + s) / (2N * a))
                Some ((-b - s) / (2N * a))
            ]
    | _ -> failwith "can't handle anything fancier than quadratic equations" // TODO

[<EntryPoint>]
let main argv =
    let v name = IVar (Variable name)
    let formula =
        // price = cost + cost * markup
        // 1 = (cost + cost * markup) / price
        IDiv(IAdd(v "cost", IMul(v "cost", v "markup")), v "price")

    let bindings =
        [   Variable "cost", Some (10N)
            Variable "markup", Some (1N/2N)
            Variable "price", None
        ] |> Map.ofList

    let plugged =
        formula
        |> plugIn bindings

    let poly = toPolynomial plugged
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
                let (v, _) = 
                    bindings
                    |> Map.toSeq
                    |> Seq.filter (snd >> Option.isNone)
                    |> Seq.exactlyOne

                printfn "%s = %O" v.Name x

    0 // return an integer exit code
