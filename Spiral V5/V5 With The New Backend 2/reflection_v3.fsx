open System
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic

type Ty =
    | IntT
    | StringT
    | ListT of Ty list

type TyToken(x: int) = class end

let dict = Dictionary()
let dict' = Dictionary()

let ty_to_int x =
    match dict.TryGetValue x with
    | true, v -> v
    | false,_ ->
        let t = dict.Count
        dict.Add(x,t)
        dict'.Add(t,x)
        t

let int_to_ty x = dict'.[x]

let assembly_name = AssemblyName("SpiralTokens")
let ab = AppDomain.CurrentDomain.DefineDynamicAssembly(assembly_name,AssemblyBuilderAccess.RunAndSave)
let mb = ab.DefineDynamicModule(assembly_name.Name)
let tb = mb.DefineType("SpiralToken")
let con_info = typeof<TyToken>.GetConstructor([|typeof<int>|])
let atrb = CustomAttributeBuilder(con_info,[|ty_to_int <| ListT [StringT; IntT]|])
let ty = tb.CreateType()
ty.CustomAttributes
let con = (ty.CustomAttributes |> Seq.head).ConstructorArguments |> Seq.head
con.Value

//