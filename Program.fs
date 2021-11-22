// Learn more about F# at http://fsharp.org

open System

//--------------------------------------------------------------------------------------------------------------------
// isNumber
//
// Return true if char is a digit between 0 and 9 and false if not.
//--------------------------------------------------------------------------------------------------------------------

let isNumber (c:char) : bool =
    match c with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'   -> true
    | _ -> false

//---------------------------------------------------------------------------------------------------------------------
// charToInt
//
// Returns single char as an integer.
// This function makes the assumption that the char passed in is a valid digit.
//---------------------------------------------------------------------------------------------------------------------

let private charToInt (c:char) : int = int c - int '0'

//---------------------------------------------------------------------------------------------------------------------
// convertStartOfStringToInt
//
// Parses a positive integer from the start of a string and then returns the integer abd the remainder of the string.
//---------------------------------------------------------------------------------------------------------------------

let convertStartOfStringToInt (s:string) : (int * string) =

    if s.Length = 0 then failwith "ERROR: Expected an integer"

    let numericChars : seq<char> = s |> Seq.takeWhile ( fun c ->  isNumber c ) 

    if Seq.length numericChars = 0 then failwith " ERROR: Expected an integer"

    //We don't expect integers of the form 02, 03 etc.
    if Seq.length numericChars > 1 && Seq.head numericChars = '0' then failwith "ERROR: Expected an integer"

    ((numericChars |> Seq.fold ( fun total x -> (charToInt x) + total * 10 ) 0), s.Substring(Seq.length numericChars))

//-----------------------------------------------------------------------------------------------------------------------
// Symbol
//
// Type to represent the various operators allowed in expressions.
//-----------------------------------------------------------------------------------------------------------------------

type Symbol =
    Open = '(' | Close = ')' | Add = '+' | Subtract = '-' | Multiply = '*' | Divide = '/'

//------------------------------------------------------------------------------------------------------------------------
// Token
//
// Type to represent everything in our expressions.  Everything must be either a Symbol or an integer.
//-------------------------------------------------------------------------------------------------------------------------
type Token = Choice<Symbol, int>

//-------------------------------------------------------------------------------------------------------------------------
// stringToTokens
//
// Recursive function to turn string into a sequence of tokens.
//-------------------------------------------------------------------------------------------------------------------------

let rec stringToTokens (s:String) : seq<Token> =

    if s.Length = 0 then Seq.empty
    else
        let first = s.Chars 0
        let rest = s.Remove( 0, 1 )

        if first = '(' then Seq.append [Choice1Of2 Symbol.Open] (stringToTokens rest)
        else if first = ')' then Seq.append [Choice1Of2 Symbol.Close] (stringToTokens rest)
        else if first = '+' then Seq.append [Choice1Of2 Symbol.Add] (stringToTokens rest)
        else if first = '-' then Seq.append [Choice1Of2 Symbol.Subtract] (stringToTokens rest)
        else if first = '*' then Seq.append [Choice1Of2 Symbol.Multiply] (stringToTokens rest)
        else if first = '/' then Seq.append [Choice1Of2 Symbol.Divide] (stringToTokens rest)
        else
            let numFromString, remains = convertStartOfStringToInt s
            Seq.append [Choice2Of2 numFromString] (stringToTokens remains)

//--------------------------------------------------------------------------------------------------------------------------
// evaluateExpression
//
// Recursive function to process a list of tokens and evaluate the expresion they represent into an integer.
//
// The function selects any sub-expressions contained within brackets first.  These can be nested so start with the last
// open brackets not the first.
//---------------------------------------------------------------------------------------------------------------------------

let rec evaluateExpression (st:seq<Token>) : int =

    if Seq.contains (Choice1Of2 Symbol.Close) st then

        if Seq.length st = 1 then failwith "ERROR: Invalid expression"

        if not (Seq.contains (Choice1Of2 Symbol.Open) st ) then failwith "ERROR: invalid expression"

        let lastOpen = Seq.findIndexBack (fun x -> x = Choice1Of2 Symbol.Open ) st      

        let after = Seq.skip (lastOpen + 1) st

        let firstCloseAfterLastOpen = Seq.findIndex (fun x -> x = Choice1Of2 Symbol.Close ) after
       
        let startOfSeq = Seq.take lastOpen st

        let endOfSeq = Seq.skip (lastOpen + 1 + firstCloseAfterLastOpen + 1) st

        let middleOfSeq = Seq.skip (lastOpen+1) (Seq.take (lastOpen + firstCloseAfterLastOpen + 1) st)

        //Process the first sub-expression found
        let middleValue = evaluateExpression middleOfSeq

        //Put the result into the original sequence and then evaluate what's left
        let newSeq = Seq.append (Seq.append startOfSeq [Choice2Of2 middleValue]) endOfSeq

        evaluateExpression newSeq

    // Divide operator has highest precedence so process this next
    else if Seq.contains (Choice1Of2 Symbol.Divide) st then

        if Seq.length st = 1 then failwith "ERROR: Invalid expression"

        let i = Seq.findIndex (fun x -> x= Choice1Of2 Symbol.Divide) st

        //The number after the divide could be a negative number so can't assume integer next
        let (secondIsNegative:bool) =
             match (Seq.item (i+1) st) with
             | Choice2Of2 x -> false
             | Choice1Of2 Symbol.Subtract -> true
             | _ -> failwith "Unreachable"

        let item = if secondIsNegative then (Seq.item (i+2) st) else (Seq.item (i+1) st)

        let (second:int) =
            match item with
            | Choice2Of2 x -> x
            | _ -> failwith "Unreachable"

        if second = 0 then failwith "ERROR: Divide by zero"

        let (first:int) =
            match (Seq.item (i-1) st) with
            | Choice2Of2 x -> x
            | _ -> failwith "Unreachable"

        // Calculate the division
        let v = if secondIsNegative then first / ( second * -1 )
                else first / second

        // Add the result back into the sequence
        let before = Seq.take (i-1) st

        let after = if secondIsNegative then Seq.skip (i+3) st
                    else Seq.skip (i+2) st

        let a = (Seq.append before [Choice2Of2 v])

        evaluateExpression (Seq.append a after)

    // Multiply operator has next highest sequence so process this next
    else if Seq.contains (Choice1Of2 Symbol.Multiply) st then

           if Seq.length st = 1 then failwith "ERROR: Invalid expression"

           let i = Seq.findIndex (fun x -> x= Choice1Of2 Symbol.Multiply) st

           //The number after the operator could be negative so process this next
           let (secondIsNegative:bool) =
                match (Seq.item (i+1) st) with
                | Choice2Of2 x -> false
                | Choice1Of2 Symbol.Subtract -> true
                | _ -> failwith "Unreachable"

           let item = if secondIsNegative then (Seq.item (i+2) st) else (Seq.item (i+1) st)

           let (second:int) =
               match item with
               | Choice2Of2 x -> x
               | _ -> failwith "Unreachable"
   
           let (first:int) =
               match (Seq.item (i-1) st) with
               | Choice2Of2 x -> x
               | _ -> failwith "Unreachable"
   
           // Calculate the multiplication
   
           let v = if secondIsNegative then first * second * -1
                   else first * second

           // Add the result back into the sequence

           let before = Seq.take (i-1) st

           let after = if secondIsNegative then Seq.skip (i+3) st
                       else Seq.skip (i+2) st
   
           let a = (Seq.append before [Choice2Of2 v]) 

           evaluateExpression (Seq.append a after)

    // Next process additions
    else if Seq.contains (Choice1Of2 Symbol.Add) st then

        if Seq.length st = 1 then failwith "ERROR: Invalid expression"

        let i = Seq.findIndex (fun x -> x= Choice1Of2 Symbol.Add) st

        let before = Seq.take i st

        let after = Seq.skip (i+1) st

        (evaluateExpression before) + (evaluateExpression after)

    // Next process subractions
    else if Seq.contains (Choice1Of2 Symbol.Subtract) st then

        let i = Seq.findIndex (fun x -> x= Choice1Of2 Symbol.Subtract ) st

        // Subtract operator may be unary
        let first =
            if i = 0 then 0
            else
                let before = Seq.take i st

                evaluateExpression before

        let after = Seq.skip (i+1) st

        first - (evaluateExpression after)

    //Finally we have an integer and the sequence can't be simplified any further
    else if Seq.length st = 1 && (match (Seq.head st) with Choice2Of2 x -> true | _ -> false) then

        let head = Seq.head st

        match head with | Choice2Of2 x -> x
                        | _ -> failwith "unreachable"

    else failwith "ERROR: expression expected"

//---------------------------------------------------------------------------------------------------------------------------
// main
//---------------------------------------------------------------------------------------------------------------------------

[<EntryPoint>]
let main argv =

    let filePath = System.IO.File.ReadLines( "/Users/mathewgore/Documents/samples.txt" );

    filePath |> Seq.iter ( fun line ->
        try

            let (ts:seq<Token>) = stringToTokens line

            let (exprResult:int) = evaluateExpression ts

            Console.WriteLine ( "<" + line + ">=<" + exprResult.ToString() + ">" )
        with
        | Failure(msg) -> Console.WriteLine( "<" + line + ">=<ERROR = " + msg + ">" )
            
    )

    0 // return an integer exit code
