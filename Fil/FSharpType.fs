module FSharpType

open System
open System.Reflection
open System.Reflection.Emit

let internal MakeRecord(typeName:string, fields:(string * Type)[]) =
    let name = "GeneratedAssembly"
    let domain = AppDomain.CurrentDomain
    let assembly = domain.DefineDynamicAssembly(AssemblyName(name), AssemblyBuilderAccess.RunAndSave)
    let name = "GeneratedModule"
    let dm = assembly.DefineDynamicModule(name, name+".dll")
    let attributes = TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.Sealed
    let typeBuilder = dm.DefineType(typeName, attributes)
    let con = typeof<CompilationMappingAttribute>.GetConstructor([|typeof<SourceConstructFlags>|])
    let customBuilder = CustomAttributeBuilder(con, [|SourceConstructFlags.RecordType|])
    typeBuilder.SetCustomAttribute(customBuilder)
    let makeField name t =
        let attributes = FieldAttributes.Assembly
        let fieldBuilder = typeBuilder.DefineField(name+"@", t, attributes)
        let attributes = PropertyAttributes.None
        let propertyBuilder = typeBuilder.DefineProperty(name, attributes, t, [||])
        let customBuilder = CustomAttributeBuilder(con, [|SourceConstructFlags.Field|])
        propertyBuilder.SetCustomAttribute(customBuilder)
        let attributes = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName
        let methodBuilder = typeBuilder.DefineMethod("get_"+name, attributes, t, [||])
        let il = methodBuilder.GetILGenerator()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, fieldBuilder)
        il.Emit(OpCodes.Ret)
        propertyBuilder.SetGetMethod(methodBuilder)
        fieldBuilder
    let types = fields |> Array.map snd
    let cb = typeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, types)
    let il = cb.GetILGenerator()
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Call, typeof<obj>.GetConstructor(Type.EmptyTypes))
    fields |> Array.iteri (fun i (name, t) -> 
        let paramName = name.Substring(0,1).ToLower()+name.Substring(1)
        let param = cb.DefineParameter(i+1, ParameterAttributes.In, paramName)
        let builder = makeField name t
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg, param.Position)
        il.Emit(OpCodes.Stfld, builder)
    )
    il.Emit(OpCodes.Ret)
    typeBuilder.CreateType()

type CaseName = string
type Field = string * Type

let internal MakeUnion (typeName:string, cases:(CaseName * Field[])[]) =
    let name = "GeneratedAssembly"
    let domain = AppDomain.CurrentDomain
    let assembly = domain.DefineDynamicAssembly(AssemblyName(name), AssemblyBuilderAccess.RunAndSave)
    let name = "GeneratedModule"
    let dm = assembly.DefineDynamicModule(name, name+".dll")
    let attributes = TypeAttributes.Public ||| TypeAttributes.Class
    let unionTypeBuilder = dm.DefineType(typeName, attributes)
    
    // Set CompilationMappingAttribute to SumType
    let con = typeof<CompilationMappingAttribute>.GetConstructor([|typeof<SourceConstructFlags>|])
    let sumType = CustomAttributeBuilder(con, [|SourceConstructFlags.SumType|])
    unionTypeBuilder.SetCustomAttribute(sumType)

    // Set Serializable
    let con = typeof<System.SerializableAttribute>.GetConstructor([||])
    let serializable = CustomAttributeBuilder(con, [||])
    unionTypeBuilder.SetCustomAttribute(serializable)
   
    // Define Tag field
    let attributes = FieldAttributes.Assembly ||| FieldAttributes.InitOnly
    let tagFieldBuilder = unionTypeBuilder.DefineField("_tag", typeof<int>, attributes)

    /// Marks property as compiler generated
    let markAsCompilerGenerated (propertyBuilder:PropertyBuilder) =
        let con = typeof<System.Runtime.CompilerServices.CompilerGeneratedAttribute>.GetConstructor([||])
        let compilerGenerated = CustomAttributeBuilder(con, [||])
        propertyBuilder.SetCustomAttribute(compilerGenerated)
        let con = typeof<System.Diagnostics.DebuggerNonUserCodeAttribute>.GetConstructor([||])
        let nonUserCode = CustomAttributeBuilder(con, [||])
        propertyBuilder.SetCustomAttribute(nonUserCode)
        let con = typeof<System.Diagnostics.DebuggerBrowsableAttribute>.GetConstructor([|typeof<System.Diagnostics.DebuggerBrowsableState>|])
        let nonBrowsable = CustomAttributeBuilder(con, [|System.Diagnostics.DebuggerBrowsableState.Never|])
        propertyBuilder.SetCustomAttribute(nonBrowsable)

    // Define Tag property
    let attributes = PropertyAttributes.None
    let tagPropertyBuilder = unionTypeBuilder.DefineProperty("Tag", attributes, typeof<int>, [||])
    markAsCompilerGenerated tagPropertyBuilder
    let attributes = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName
    let tagMethodBuilder = unionTypeBuilder.DefineMethod("get_Tag", attributes, typeof<int>, [||])
    let con = typeof<System.Diagnostics.DebuggerNonUserCodeAttribute>.GetConstructor([||])
    let nonUserCode = CustomAttributeBuilder(con, [||])
    tagMethodBuilder.SetCustomAttribute(nonUserCode)
    let il = tagMethodBuilder.GetILGenerator()
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Ldfld, tagFieldBuilder)
    il.Emit(OpCodes.Ret)
    tagPropertyBuilder.SetGetMethod(tagMethodBuilder)

    // Define union constructor
    let unionTypeConstructor = 
        unionTypeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [|typeof<int>|])
    let il = unionTypeConstructor.GetILGenerator()
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Call, typeof<obj>.GetConstructor(Type.EmptyTypes))
    let param = unionTypeConstructor.DefineParameter(1, ParameterAttributes.In, "tag")
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Ldarg, param.Position)
    il.Emit(OpCodes.Stfld, tagFieldBuilder)
    il.Emit(OpCodes.Ret)

    /// Cases
    let cases = cases |> Array.mapi (fun tag (name,fields) -> tag,name,fields)

    // Define tags
    let attributes = TypeAttributes.Class ||| TypeAttributes.NestedPublic ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
    let tagsBuilder = unionTypeBuilder.DefineNestedType("Tags", attributes)
    for tag,name,_ in cases do
        let attributes = FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.Literal
        let tagFieldBuilder = tagsBuilder.DefineField(name, typeof<int>, attributes)
        tagFieldBuilder.SetConstant(tag)
    tagsBuilder.CreateType() |> ignore

    // Define case properties
    cases 
    |> Array.filter (fun (_,_,fields) -> fields.Length = 0)
    |> Array.iter (fun (tag, caseName, _) ->
        // Generate property
        let attributes = PropertyAttributes.None
        let propertyBuilder = 
            unionTypeBuilder.DefineProperty(caseName, attributes, CallingConventions.Standard, unionTypeBuilder, [||])
        markAsCompilerGenerated propertyBuilder
        // Generate getter method
        let attributes = MethodAttributes.Public ||| MethodAttributes.Static
        let methodBuilder = unionTypeBuilder.DefineMethod("get_"+caseName, attributes, unionTypeBuilder, [||])
        let con = typeof<CompilationMappingAttribute>.GetConstructor([|typeof<SourceConstructFlags>;typeof<int>|])
        let customBuilder = CustomAttributeBuilder(con, [|SourceConstructFlags.UnionCase; tag|])
        methodBuilder.SetCustomAttribute(customBuilder)
        let il = methodBuilder.GetILGenerator()
        il.Emit(OpCodes.Ldc_I4, tag)
        il.Emit(OpCodes.Newobj, unionTypeConstructor)
        il.Emit(OpCodes.Ret)
        // Set get method
        propertyBuilder.SetGetMethod(methodBuilder)
    )

    // Define case types
    let caseTypes =
        cases 
        |> Array.filter (fun (_,_,fields) -> fields.Length > 0) 
        |> Array.map (fun (tag,name,fields) ->
            let attributes = TypeAttributes.Class ||| TypeAttributes.NestedPublic ||| TypeAttributes.SpecialName
            let caseBuilder = unionTypeBuilder.DefineNestedType(name, attributes)
            // Define items
            let items =
                fields |> Array.mapi (fun i (name, t) ->
                    // Define field
                    let fieldName = name.Substring(0,1).ToLower()+name.Substring(1)
                    let attributes = FieldAttributes.Assembly ||| FieldAttributes.InitOnly
                    let itemFieldBuilder = caseBuilder.DefineField(fieldName, t, attributes)
                    // Define property
                    let attributes = PropertyAttributes.None
                    let propertyBuilder = caseBuilder.DefineProperty(name, attributes, CallingConventions.HasThis, t, [||])
                    let con = typeof<CompilationMappingAttribute>.GetConstructor([|typeof<SourceConstructFlags>;typeof<int>;typeof<int>|])
                    let customBuilder = CustomAttributeBuilder(con, [|SourceConstructFlags.Field; tag; i|])
                    propertyBuilder.SetCustomAttribute(customBuilder)
                    // Define getter
                    let attributes = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName
                    let methodBuilder = caseBuilder.DefineMethod("get_"+name, attributes, t, [||])
                    let il = methodBuilder.GetILGenerator()
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Ldfld, itemFieldBuilder)
                    il.Emit(OpCodes.Ret)
                    propertyBuilder.SetGetMethod(methodBuilder)
                    itemFieldBuilder
                )
            // Define constructor
            let types = fields |> Array.map snd
            let cb = caseBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, types)
            fields |> Array.iteri (fun i (name,_) -> 
                let paramName = name.Substring(0,1).ToLower()+name.Substring(1)
                cb.DefineParameter(i+1, ParameterAttributes.In, paramName) |> ignore
            )
            let il = cb.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Call, typeof<obj>.GetConstructor(Type.EmptyTypes))
            items |> Array.iteri (fun i item ->
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldarg, i+1)
                il.Emit(OpCodes.Stfld, item)
            )
            il.Emit(OpCodes.Ret)
            tag, name, fields, caseBuilder, cb
        )

    // Define case new methods
    for tag, caseName, fields, caseBuilder, cb in caseTypes do
        let types = fields |> Array.map snd
        let attributes = MethodAttributes.Public ||| MethodAttributes.Static
        let methodBuilder = unionTypeBuilder.DefineMethod("New"+caseName, attributes, unionTypeBuilder, types)
        let con = typeof<CompilationMappingAttribute>.GetConstructor([|typeof<SourceConstructFlags>;typeof<int>|])
        let customBuilder = CustomAttributeBuilder(con, [|SourceConstructFlags.UnionCase; tag|])
        methodBuilder.SetCustomAttribute(customBuilder)
        let il = methodBuilder.GetILGenerator()
        types |> Array.iteri (fun i t -> il.Emit(OpCodes.Ldarg, i))
        il.Emit(OpCodes.Newobj, cb)
        il.Emit(OpCodes.Ret)
    
    /// Parent type
    let parent = unionTypeBuilder.CreateType()

    // Create case types
    for _,_,_,caseBuilder,_ in caseTypes do 
        caseBuilder.SetParent(parent)
        caseBuilder.CreateType() |> ignore

    #if DEBUG
    assembly.Save("GeneratedModule.dll")
    #endif

    parent