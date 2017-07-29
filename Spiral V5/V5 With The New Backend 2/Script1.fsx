let a = ref 0
let f (x: byref<int>) = x

f a // type error

System.Int32.TryParse("123",a) // works
