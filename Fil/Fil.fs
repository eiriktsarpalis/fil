module Fil

type Microsoft.FSharp.Reflection.FSharpType with
    /// Returns a Type representing an F# record type with the given fields
    static member MakeRecordType(name,fields) = 
        FSharpType.MakeRecord(name,fields)
    /// Returns a Type representing an F# union type with the given cases
    static member MakeUnionType(name,cases) = 
        let cases =
            [|for name, types in cases ->
                name,
                    match types with
                    | [|single|] -> [|"Item",single|]
                    | _ -> types |> Array.mapi (fun i t -> sprintf "Item%d" (i+1), t)
            |]
        FSharpType.MakeUnion(name, cases)

open FSharpFun
open System
open System.Reflection.Emit
open Microsoft.FSharp.Quotations

type internal Marker = interface end
let Compile (quotation:Expr<'T>) =
    let dyn = DynamicMethod("f", typeof<'T>, [||], typeof<Marker>.Module, skipVisibility = true)
    let il = dyn.GetILGenerator()
    quotation |> generate [] il
    il.Emit OpCodes.Ret
    let func = dyn.CreateDelegate(typeof<Func<'T>>) :?> Func<'T>
    func.Invoke

let CompileUntyped (quotation:Expr) =
    let expr = Expr.Cast<obj>(Expr.Coerce(quotation, typeof<obj>))
    Compile expr