[<ReflectedDefinition>]
module Examples

let ``null`` () : obj = null
let ``unit`` () = ()
let ``1`` () = 1
let ``9`` () = 9
let ``-1`` () = -1
let ``-2`` () = -2
let ``2uy`` () = 2uy
let ``2y`` () = 2y
let ``2us`` () = 2us
let ``2s`` () = 2s
let ``2L`` () = 2L
let ``2UL`` () = 2UL
let ``2.0`` () = 2.0
let ``2.0f`` () = 2.0f
let ``2M`` () = 2M
let ``"Hello"`` () = "Hello"
let ``1 + 1`` () = 1 + 1
let ``+ 1 1`` () = (+) 1 1
let ``"A" + "B"`` () = "A" + "B"
let ``200uy + 200uy`` () = 200uy + 200uy
let ``127y + 127y`` () = 127y + 127y
let ``60000us + 60000us`` () = 60000us + 60000us
let ``32000s + 32000s`` () = 32000s + 32000s
let ``1 + 1 = 2`` () = 1 + 1 = 2
let ``1 < 2`` () = 1 < 2
let ``1 > 2`` () = 1 > 2
let ``2 <= 2`` () = 2 <= 2
let ``mod`` () = 5 % 4
let ``**`` () = 2. ** 4.
let ``pown`` () = pown 3.0 2
let ``1 <<< 3`` () = 1 <<< 3
let ``8 >>> 3`` () = 8 >>> 3
let ``not false`` () = not false
let ``not true`` () = not true
let ``true and true`` () = true && true
let ``true and false`` () = true && false
let ``true or false`` () = let a,b = true,false in a || b
let ``max`` () = System.Math.Max(0,1)
let ``new`` () = System.DateTime(1L)
let ``[1..3]`` () = [1..3]
let ``Array`` () = [|1;2;3|]
let ``[|x..y|]`` () = [|1..3|]
let ``Array get`` () = let xs = [|1;2;3|] in xs.[0]
let ``Array set`` () = let xs = [|1;2;3|] in xs.[1] <- -2; xs
let ``tuple/2`` () = 1,2
let ``tuple/3`` () = 1,2,3
let ``tuple get`` () = let _,two = 1,2 in two
let ``property get`` () = System.Math.PI
let ``property set`` () = let x = System.Collections.Generic.List<int>() in x.Capacity <- 10; x.Capacity
let ``new record`` () = { MyRecord.A = 1; B="One" }
let ``record field`` () = let x = { MyRecord.A = 1; B="One" } in x.A
let ``none`` () : int option = None
let ``some`` () = Some 1
let ``matching`` () = let x = Some(1) in match x with Some x -> x | None -> 0
let ``generic typetest true`` () = box 1 :? int
let ``generic typetest false`` () = box 1 :? decimal
let ``typetest`` () = match box 1 with :? int -> true | _ -> false
let ``coerce`` () = [|1|] :> System.Collections.IEnumerable
let ``let`` () = let x = 1 in x + 1
let ``let2`` () = let a = 2 in let b = 3 in a * b
let ``sequential`` () = (); (); (); 3
let ``then`` () = if true then 1 else 0
let ``else`` () = if false then 1 else 0
let ``nested if`` () =
    if true then
        if true then 1
        else 0
    else -1
let ``mutable`` () = let mutable x = 1 in x <- x + 1; x
let ``for loop`` () = 
    let mutable sum = 0 
    for n = 1 to 3 do sum <- sum + n
    sum
let ``while loop`` () = 
    let mutable x = 3 
    while x > 0 do x <- x - 1
    x
let ``locals`` () =
    let a = 1
    let b = 2
    let c = 3
    let d = 4
    let e = 5
    a + b + c + d + e

let ``match`` () =
    let a  = 0
    match a with
    | 0 -> "Zero"
    | 1 -> "One"
    | n -> n.ToString()