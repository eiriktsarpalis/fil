module Tests

open Fil
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open NUnit.Framework
open Microsoft.FSharp.Reflection

[<TestCaseSource("examples")>]
let test (methodInfo:MethodInfo) =
    let expr =
        match Expr.TryGetReflectedDefinition(methodInfo) with
        | Some(Lambda(unit,expr)) -> expr
        | Some(_) -> failwith "expecting lambda"
        | None -> invalidOp "expecting reflected definition"
    let f = CompileUntyped(expr, methodInfo.ReturnType)
    let expected = methodInfo.Invoke(null, [||])
    let actual = f ()
    Assert.AreEqual(expected, actual)

type internal Marker = interface end 
let examples () = seq {
    for t in typeof<Marker>.Assembly.GetTypes() do
        for methodInfo in t.GetMethods() do
            match Expr.TryGetReflectedDefinition(methodInfo) with
            | Some(expr) -> yield methodInfo
            | _ -> ()
    }