module Formulas.Main

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

    let solutions = Solver.solveEquation freeVariable equation

    match solutions with
    | [] -> printfn "no solutions!"
    | solutions ->
        for x in solutions do
            printfn "%O = %O" freeVariable x
            let answer = x.Approximate(fun v -> Map.find v values)
            printfn "%O = %O" freeVariable answer

    0 // return an integer exit code
