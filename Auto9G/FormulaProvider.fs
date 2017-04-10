namespace Formulas.Internals.ProviderImplementation
open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Formulas.Internals

module private ProviderUtilities =
    let rec toQuotation (variables : Variable -> Expr<decimal>) (solution : Solution) =
        match solution with
        | Literal i -> Expr.Cast(Expr.Value(decimal i))
        | BoundVariable v -> variables v
        | Multiply (x, y) -> <@ %toQuotation variables x * %toQuotation variables y @>
        | Divide (x, y) -> <@ %toQuotation variables x / %toQuotation variables y @>
        | Add (x, y) -> <@ %toQuotation variables x + %toQuotation variables y @>
        | SquareRoot x -> <@ decimal(Math.Sqrt(double (%toQuotation variables x))) @>
        | CubeRoot x-> <@ decimal(Math.Pow(double (%toQuotation variables x), 1.0/3.0)) @>
open ProviderUtilities

[<TypeProvider>]
type Provider(cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()
    let namespaceName = "Formulas.Provider"
    let thisAssembly = Assembly.GetExecutingAssembly()

    let formulaTy =
        ProvidedTypeDefinition
            ( thisAssembly
            , namespaceName
            , "Formula"
            , Some typeof<obj>
            , HideObjectMethods = true
            )

    do
        formulaTy.DefineStaticParameters
            ( [ ProvidedStaticParameter("formula", typeof<string>) ]
            , fun typeName parameters ->
                match parameters with
                | [| :? string as formula |] ->
                    let ty = ProvidedTypeDefinition(thisAssembly, namespaceName, typeName, Some typeof<obj>)
                    let equation = Parser.run Parser.equation formula
                    let allVariables = equation.Variables() |> Seq.distinct |> Seq.toArray
                    for variable in allVariables do
                        let solutions = Solver.solveEquation variable equation |> List.map (fun s -> s.Simplify())
                        if solutions |> List.isEmpty then () else
                        let otherVariables =
                            allVariables |> Array.filter ((<>) variable) |> Array.sortBy (fun v -> v.Name)
                        let parameters =
                            [ for otherVariable in otherVariables ->
                                ProvidedParameter(otherVariable.Name, typeof<decimal>)
                            ]
                        let methodType =
                            match solutions with
                            | [ single ] -> typeof<decimal>
                            | _ -> typeof<(unit -> decimal) array>
                        let method = ProvidedMethod(variable.Name, parameters, methodType)
                        method.AddXmlDocDelayed(fun () ->
                            let by = solutions |> Seq.map string |> String.concat " or as "
                            "Calculates " + variable.Name + " as " + by)
                        method.SetMethodAttrs(MethodAttributes.Static ||| MethodAttributes.Public)
                        method.InvokeCode <-
                            fun pars ->
                                let boundVariables =
                                    (otherVariables, pars)
                                    ||> Seq.map2 (fun var p -> var, Expr.Cast(p))
                                    |> Map.ofSeq
                                    |> fun m k -> Map.find k m
                                match solutions with
                                | [ single ] ->
                                    upcast toQuotation boundVariables single
                                | solutions ->
                                    let solutionExprs =
                                        [ for solution in solutions ->
                                            let body = toQuotation boundVariables solution
                                            Expr.Lambda(Var("unit", typeof<unit>), body)
                                        ]
                                    Expr.NewArray(typeof<unit -> decimal>, solutionExprs)
                        ty.AddMember(method)
                    ty
                | _ ->
                    failwith "Invalid static parameters"
            )
        this.AddNamespace(namespaceName, [ formulaTy ])

[<TypeProviderAssembly>]
do ()