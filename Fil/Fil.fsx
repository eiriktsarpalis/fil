#load "FSharpType.fs"
#load "FSharpFun.fs"
#load "Fil.fs"
open Fil

let f = Compile <@ 2. + 2. > 3. @>
let x = f ()