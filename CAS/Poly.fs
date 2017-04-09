namespace Formulas

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

module PolyUtilities =
    let power (PolyTerm (_, p)) = p
    let coeff (PolyTerm (c, _)) = c
open PolyUtilities

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