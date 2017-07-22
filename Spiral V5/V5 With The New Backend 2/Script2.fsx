    open System
    let asm = Reflection.Assembly.Load("mscorlib")
    let dic = asm.GetType("System.Collections.Generic.Dictionary`2")