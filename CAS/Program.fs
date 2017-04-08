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

type Number =
    | Literal of int64
    | Multiply of Number * Number
    | Divide of Number * Number
    | Add of Number * Number
    | SquareRoot of Number
    | CubeRoot of Number
    static member Zero =
        Literal 0L
    static member Sqrt(x) =
        SquareRoot x
    static member ( * ) (l, r) =
        Multiply (l, r)
    static member ( / ) (l, r) =
        Divide (l, r)
    static member ( ~- ) (x) =
        Multiply(Literal -1L, x)
    static member ( + ) (l, r) =
        Add (l, r)
    static member ( - ) (l, r) =
        Add (l, -r)
    member this.Approximate() =
        match this with
        | Literal x -> decimal x
        | Multiply (x, y) ->
            x.Approximate() * y.Approximate()
        | Divide (x, y) ->
            x.Approximate() / y.Approximate()
        | Add (x, y) ->
            x.Approximate() + y.Approximate()
        | SquareRoot x ->
            decimal (sqrt (double (x.Approximate())))
        | CubeRoot x ->
            decimal (Math.Pow(double (x.Approximate()), 1.0/3.0))
    member this.ToString(showApproximate : bool) =
        let simple =
            match this with
            | Literal x -> string x
            | Multiply (x, y) -> sprintf "(%s*%s)" (x.ToString(false)) (y.ToString(false))
            | Divide (x, y) -> sprintf "(%s/%s)" (x.ToString(false)) (y.ToString(false))
            | Add (x, y) -> sprintf "(%s+%s)" (x.ToString(false)) (y.ToString(false))
            | SquareRoot x -> sprintf "sqrt(%s)" (x.ToString(false))
            | CubeRoot x -> sprintf "(%s^1/3)" (x.ToString(false))
        if showApproximate then
            sprintf "~%O: " (this.Approximate()) + simple
        else
            simple
    override this.ToString() =
        this.ToString(showApproximate = true)

module NumericLiteralN =
    let FromOne () = Literal 1L
    let FromZero () = Literal 0L
    let FromInt32 i = Literal (int64 i)
    let FromInt64 i = Literal i
    let FromString (s : string) = Literal (int64 s)
    
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
