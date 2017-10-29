open System
let x = Reflection.Assembly.Load("mscorlib")
let dic' = x.GetType("System.Collections.Generic.Dictionary`2")
let dic_met = dic'.GetMethods()
dic_met.[0].IsStatic

let str = x.GetType("System.String")
let str_met = str.GetMethods()
str_met.[0].IsStatic

let int = x.GetType("System.Int32")
let int_fld = int.GetFields()
int_fld.[0].IsStatic