module Formulas.Parser

type ParseResult<'a> =
    | Success of result : 'a * pos : int
    | Fail of pos : int * msg : string

type Parser<'a> = string -> int -> ParseResult<'a>

let opt (p : Parser<'a>) =
    fun input pos ->
        match p input pos with
        | Fail (newPos, msg) when newPos > pos -> Fail (newPos, msg)
        | Fail _ -> Success(None, pos)
        | Success (res, pos) -> Success(Some res, pos)

let (|>>) (p : Parser<'a>) f =
    fun input pos ->
        match p input pos with
        | Fail (pos, msg) -> Fail (pos, msg)
        | Success (res, pos) -> Success (f res, pos)

let (<|>) (p1 : Parser<'a>) (p2 : Parser<'a>) =
    fun input pos ->
        match p1 input pos with
        | Success _ as succ -> succ
        | Fail (newPos, _) as fail when newPos > pos -> fail
        | Fail (_, msg1) ->
            match p2 input pos with
            | Success _ as succ -> succ
            | Fail (newPos, msg2) ->
                Fail (newPos, msg1 + " | " + msg2)

let pipe2 (p1 : Parser<'a>) (p2 : Parser<'b>) f =
    fun input pos ->
        match p1 input pos with
        | Fail (pos, msg) -> Fail (pos, msg)
        | Success (res1, pos) ->
            match p2 input pos with
            | Fail (pos, msg) -> Fail (pos, msg)
            | Success (res2, pos) ->
                Success (f res1 res2, pos)

let (.>>) (p1 : Parser<'a>) (p2 : Parser<'b>) =
    pipe2 p1 p2 (fun a _ -> a)

let (>>.) (p1 : Parser<'a>) (p2 : Parser<'b>) =
    pipe2 p1 p2 (fun _ b -> b)

let pstring (str : string) (input : string) pos =
    let mutable currentPos = pos
    while (
            currentPos < input.Length 
            && currentPos - pos < str.Length
            && input.[currentPos] = str.[currentPos - pos]
    ) do
        currentPos <- currentPos + 1
    if currentPos - pos = str.Length then
        Success((), currentPos)
    else
        Fail (currentPos, "expected " + str)

let variable (input : string) pos =
    let mutable currentPos = pos
    let inline valid c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
    while currentPos < input.Length && valid input.[currentPos] do
        currentPos <- currentPos + 1
    if currentPos > pos then
        let substr = input.Substring(pos, currentPos - pos)
        Success (InputVar (Variable substr), currentPos)
    else
        Fail (currentPos, "expected variable [a-z]")
            
let number (input : string) pos =
    let mutable currentPos = pos
    let inline valid c = c >= '0' && c <= '9'
    while currentPos < input.Length && valid input.[currentPos] do
            currentPos <- currentPos + 1
    if currentPos > pos then
        let substr = input.Substring(pos, currentPos - pos)
        Success (InputNum (Literal (int64 substr)), currentPos)
    else
        Fail (currentPos, "expected number [0-9]")

let ws (input : string) pos =
    let mutable currentPos = pos
    let inline valid c = c = ' ' || c = '\t' || c = '\r' || c = '\n'
    while currentPos < input.Length && valid input.[currentPos] do
        currentPos <- currentPos + 1
    Success ((), currentPos)

let simpleTerm = (variable <|> number) .>> ws

let parenthesized expr =
    pstring "(" >>. ws >>. expr .>> pstring ")" .>> ws

let term expr =
    parenthesized expr <|> simpleTerm

let expr =
    let mutable mutableSelf = Unchecked.defaultof<InputExpr Parser>
    let expr =
        fun input pos -> mutableSelf input pos
    let level1 =
        let term = term expr
        (pstring "-" >>. ws >>. term |>> InputNeg)
        <|> term
    let applyOp left f =
        match f with
        | None -> left
        | Some f -> f left
    let mul = pstring "*" >>. ws >>. level1 |>> fun r l -> InputMul (l, r)
    let div = pstring "/" >>. ws >>. level1 |>> fun r l -> InputDiv (l, r)
    let level2 =
        pipe2 level1 (opt (mul <|> div)) applyOp
    let add = pstring "+" >>. ws >>. level2 |>> fun r l -> InputAdd (l, r)
    let sub = pstring "-" >>. ws >>. level2 |>> fun r l -> InputSub (l, r)
    let level3 =
        pipe2 level2 (opt (add <|> sub)) applyOp
    let anyOperator = mul <|> div <|> add <|> sub
    mutableSelf <-
        fun input pos ->
            match level1 input pos with
            | Fail _ as fail -> fail
            | Success (leftTerm, pos) as succ ->
                if pos >= input.Length then succ else
                let mutable pos = pos
                let mutable looping = true
                let mutable acc = leftTerm
                let mutable failure = None
                while looping do
                    match anyOperator input pos with
                    | Fail (newPos, msg) when newPos > pos ->
                        looping <- false
                        failure <- Some (Fail (newPos, msg))
                    | Fail _ ->
                        looping <- false
                    | Success (op, newPos) ->
                        acc <- op acc
                        pos <- newPos
                match failure with
                | None -> Success(acc, pos)
                | Some f -> f     
    mutableSelf
