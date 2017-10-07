type Union0 =
 | Union0Case0
 | Union0Case1
 | Union0Case2
and Tuple1 =
 struct
 val mem_0: Tuple4
 val mem_1: Rec3
 new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
 end
and Env2 =
 struct
 val mem_ar: (Tuple1 [])
 val mem_from: int64
 val mem_to: int64
 new(arg_mem_ar, arg_mem_from, arg_mem_to) = {mem_ar = arg_mem_ar; mem_from = arg_mem_from; mem_to = arg_mem_to}
 end
and Rec3 =
 | Rec3Case0
 | Rec3Case1 of Tuple5
and Tuple4 =
 struct
 val mem_0: int64
 val mem_1: int64
 new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
 end
and Tuple5 =
 struct
 val mem_0: string
 val mem_1: Rec3
 new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
 end
let rec method_19((var_0: bool), (var_1: string), (var_2: int64)): unit =
 let (var_4: bool) =
  if (var_2 >= 0L) then
   let (var_3: int64) = (int64 var_1.Length)
   (var_2 < var_3)
  else
   false
 if var_4 then
  let (var_5: char) = var_1.[int32 var_2]
  let (var_6: bool) =
   if (var_5 >= '0') then
    (var_5 <= '9')
   else
    false
  let (var_7: int64) = (var_2 + 1L)
  if var_6 then
   let (var_8: int64) = System.Convert.ToInt64(var_5)
   let (var_9: int64) = System.Convert.ToInt64('0')
   let (var_10: int64) = (var_8 - var_9)
   let (var_11: int64) = (0L + var_10)
   method_20((var_0: bool), (var_1: string), (var_11: int64), (var_7: int64))
  else
   (failwith "pint64")
 else
  (failwith "pint64")
and method_20((var_0: bool), (var_1: string), (var_2: int64), (var_3: int64)): unit =
 let (var_5: bool) =
  if (var_3 >= 0L) then
   let (var_4: int64) = (int64 var_1.Length)
   (var_3 < var_4)
  else
   false
 if var_5 then
  let (var_6: char) = var_1.[int32 var_3]
  let (var_7: bool) =
   if (var_6 >= '0') then
    (var_6 <= '9')
   else
    false
  let (var_8: int64) = (var_3 + 1L)
  if var_7 then
   let (var_9: int64) = System.Convert.ToInt64(var_6)
   let (var_10: int64) = System.Convert.ToInt64('0')
   let (var_11: int64) = (var_9 - var_10)
   let (var_12: bool) =
    if (var_2 = 922337203685477580L) then
     (var_11 <= 7L)
    else
     false
   let (var_13: bool) =
    if var_12 then
     true
    else
     (var_2 < 922337203685477580L)
   if var_13 then
    let (var_14: int64) = (var_2 * 10L)
    let (var_15: int64) = (var_14 + var_11)
    method_20((var_0: bool), (var_1: string), (var_15: int64), (var_8: int64))
   else
    (failwith "integer overflow")
  else
   let (var_16: int64) =
    if var_0 then
     var_2
    else
     (-var_2)
   let (var_17: int64) = 0L
   method_21((var_16: int64), (var_1: string), (var_17: int64), (var_3: int64))
 else
  let (var_18: int64) =
   if var_0 then
    var_2
   else
    (-var_2)
  let (var_19: int64) = 0L
  method_21((var_18: int64), (var_1: string), (var_19: int64), (var_3: int64))
and method_21((var_0: int64), (var_1: string), (var_2: int64), (var_3: int64)): unit =
 let (var_4: int64) = (var_2 + 1L)
 let (var_6: bool) =
  if (var_3 >= 0L) then
   let (var_5: int64) = (int64 var_1.Length)
   (var_3 < var_5)
  else
   false
 if var_6 then
  let (var_7: char) = var_1.[int32 var_3]
  let (var_9: bool) =
   if (var_7 = ' ') then
    true
   else
    if (var_7 = '\n') then
     true
    else
     (var_7 = '\r')
  let (var_10: int64) = (var_3 + 1L)
  if var_9 then
   method_21((var_0: int64), (var_1: string), (var_4: int64), (var_10: int64))
  else
   let (var_12: bool) =
    if (var_3 >= 0L) then
     let (var_11: int64) = (int64 var_1.Length)
     (var_3 < var_11)
    else
     false
   if var_12 then
    let (var_13: bool) = ('-' = var_7)
    if var_13 then
     let (var_14: bool) = false
     method_22((var_14: bool), (var_0: int64), (var_1: string), (var_10: int64))
    else
     let (var_15: bool) = true
     method_22((var_15: bool), (var_0: int64), (var_1: string), (var_3: int64))
   else
    let (var_16: bool) = true
    method_22((var_16: bool), (var_0: int64), (var_1: string), (var_3: int64))
 else
  let (var_18: bool) =
   if (var_3 >= 0L) then
    let (var_17: int64) = (int64 var_1.Length)
    (var_3 < var_17)
   else
    false
  if var_18 then
   let (var_19: char) = var_1.[int32 var_3]
   let (var_20: bool) = ('-' = var_19)
   let (var_21: int64) = (var_3 + 1L)
   if var_20 then
    let (var_22: bool) = false
    method_22((var_22: bool), (var_0: int64), (var_1: string), (var_21: int64))
   else
    let (var_23: bool) = true
    method_22((var_23: bool), (var_0: int64), (var_1: string), (var_3: int64))
  else
   let (var_24: bool) = true
   method_22((var_24: bool), (var_0: int64), (var_1: string), (var_3: int64))
and method_22((var_0: bool), (var_1: int64), (var_2: string), (var_3: int64)): unit =
 let (var_5: bool) =
  if (var_3 >= 0L) then
   let (var_4: int64) = (int64 var_2.Length)
   (var_3 < var_4)
  else
   false
 if var_5 then
  let (var_6: char) = var_2.[int32 var_3]
  let (var_7: bool) =
   if (var_6 >= '0') then
    (var_6 <= '9')
   else
    false
  let (var_8: int64) = (var_3 + 1L)
  if var_7 then
   let (var_9: int64) = System.Convert.ToInt64(var_6)
   let (var_10: int64) = System.Convert.ToInt64('0')
   let (var_11: int64) = (var_9 - var_10)
   let (var_12: int64) = (0L + var_11)
   method_23((var_0: bool), (var_1: int64), (var_2: string), (var_12: int64), (var_8: int64))
  else
   (failwith "pint64")
 else
  (failwith "pint64")
and method_23((var_0: bool), (var_1: int64), (var_2: string), (var_3: int64), (var_4: int64)): unit =
 let (var_6: bool) =
  if (var_4 >= 0L) then
   let (var_5: int64) = (int64 var_2.Length)
   (var_4 < var_5)
  else
   false
 if var_6 then
  let (var_7: char) = var_2.[int32 var_4]
  let (var_8: bool) =
   if (var_7 >= '0') then
    (var_7 <= '9')
   else
    false
  let (var_9: int64) = (var_4 + 1L)
  if var_8 then
   let (var_10: int64) = System.Convert.ToInt64(var_7)
   let (var_11: int64) = System.Convert.ToInt64('0')
   let (var_12: int64) = (var_10 - var_11)
   let (var_13: bool) =
    if (var_3 = 922337203685477580L) then
     (var_12 <= 7L)
    else
     false
   let (var_14: bool) =
    if var_13 then
     true
    else
     (var_3 < 922337203685477580L)
   if var_14 then
    let (var_15: int64) = (var_3 * 10L)
    let (var_16: int64) = (var_15 + var_12)
    method_23((var_0: bool), (var_1: int64), (var_2: string), (var_16: int64), (var_9: int64))
   else
    (failwith "integer overflow")
  else
   let (var_17: int64) =
    if var_0 then
     var_3
    else
     (-var_3)
   let (var_18: int64) = 0L
   method_24((var_17: int64), (var_1: int64), (var_2: string), (var_18: int64), (var_4: int64))
 else
  let (var_19: int64) =
   if var_0 then
    var_3
   else
    (-var_3)
  let (var_20: int64) = 0L
  method_24((var_19: int64), (var_1: int64), (var_2: string), (var_20: int64), (var_4: int64))
and method_24((var_0: int64), (var_1: int64), (var_2: string), (var_3: int64), (var_4: int64)): unit =
 let (var_5: int64) = (var_3 + 1L)
 let (var_7: bool) =
  if (var_4 >= 0L) then
   let (var_6: int64) = (int64 var_2.Length)
   (var_4 < var_6)
  else
   false
 if var_7 then
  let (var_8: char) = var_2.[int32 var_4]
  let (var_10: bool) =
   if (var_8 = ' ') then
    true
   else
    if (var_8 = '\n') then
     true
    else
     (var_8 = '\r')
  let (var_11: int64) = (var_4 + 1L)
  if var_10 then
   method_24((var_0: int64), (var_1: int64), (var_2: string), (var_5: int64), (var_11: int64))
  else
   let (var_13: bool) =
    if (var_4 >= 0L) then
     let (var_12: int64) = (int64 var_2.Length)
     (var_4 < var_12)
    else
     false
   if var_13 then
    let (var_14: bool) = ('-' = var_8)
    if var_14 then
     let (var_15: bool) = false
     method_25((var_15: bool), (var_1: int64), (var_2: string), (var_11: int64))
    else
     let (var_16: bool) = true
     method_25((var_16: bool), (var_1: int64), (var_2: string), (var_4: int64))
   else
    let (var_17: bool) = true
    method_25((var_17: bool), (var_1: int64), (var_2: string), (var_4: int64))
 else
  let (var_19: bool) =
   if (var_4 >= 0L) then
    let (var_18: int64) = (int64 var_2.Length)
    (var_4 < var_18)
   else
    false
  if var_19 then
   let (var_20: char) = var_2.[int32 var_4]
   let (var_21: bool) = ('-' = var_20)
   let (var_22: int64) = (var_4 + 1L)
   if var_21 then
    let (var_23: bool) = false
    method_25((var_23: bool), (var_1: int64), (var_2: string), (var_22: int64))
   else
    let (var_24: bool) = true
    method_25((var_24: bool), (var_1: int64), (var_2: string), (var_4: int64))
  else
   let (var_25: bool) = true
   method_25((var_25: bool), (var_1: int64), (var_2: string), (var_4: int64))
and method_25((var_0: bool), (var_1: int64), (var_2: string), (var_3: int64)): unit =
 let (var_5: bool) =
  if (var_3 >= 0L) then
   let (var_4: int64) = (int64 var_2.Length)
   (var_3 < var_4)
  else
   false
 if var_5 then
  let (var_6: char) = var_2.[int32 var_3]
  let (var_7: bool) =
   if (var_6 >= '0') then
    (var_6 <= '9')
   else
    false
  let (var_8: int64) = (var_3 + 1L)
  if var_7 then
   let (var_9: int64) = System.Convert.ToInt64(var_6)
   let (var_10: int64) = System.Convert.ToInt64('0')
   let (var_11: int64) = (var_9 - var_10)
   let (var_12: int64) = (0L + var_11)
   method_26((var_0: bool), (var_1: int64), (var_2: string), (var_12: int64), (var_8: int64))
  else
   (failwith "pint64")
 else
  (failwith "pint64")
and method_26((var_0: bool), (var_1: int64), (var_2: string), (var_3: int64), (var_4: int64)): unit =
 let (var_6: bool) =
  if (var_4 >= 0L) then
   let (var_5: int64) = (int64 var_2.Length)
   (var_4 < var_5)
  else
   false
 if var_6 then
  let (var_7: char) = var_2.[int32 var_4]
  let (var_8: bool) =
   if (var_7 >= '0') then
    (var_7 <= '9')
   else
    false
  let (var_9: int64) = (var_4 + 1L)
  if var_8 then
   let (var_10: int64) = System.Convert.ToInt64(var_7)
   let (var_11: int64) = System.Convert.ToInt64('0')
   let (var_12: int64) = (var_10 - var_11)
   let (var_13: bool) =
    if (var_3 = 922337203685477580L) then
     (var_12 <= 7L)
    else
     false
   let (var_14: bool) =
    if var_13 then
     true
    else
     (var_3 < 922337203685477580L)
   if var_14 then
    let (var_15: int64) = (var_3 * 10L)
    let (var_16: int64) = (var_15 + var_12)
    method_26((var_0: bool), (var_1: int64), (var_2: string), (var_16: int64), (var_9: int64))
   else
    (failwith "integer overflow")
  else
   let (var_17: int64) =
    if var_0 then
     var_3
    else
     (-var_3)
   let (var_18: int64) = 0L
   method_27((var_17: int64), (var_1: int64), (var_2: string), (var_18: int64), (var_4: int64))
 else
  let (var_19: int64) =
   if var_0 then
    var_3
   else
    (-var_3)
  let (var_20: int64) = 0L
  method_27((var_19: int64), (var_1: int64), (var_2: string), (var_20: int64), (var_4: int64))
and method_27((var_0: int64), (var_1: int64), (var_2: string), (var_3: int64), (var_4: int64)): unit =
 let (var_5: int64) = (var_3 + 1L)
 let (var_7: bool) =
  if (var_4 >= 0L) then
   let (var_6: int64) = (int64 var_2.Length)
   (var_4 < var_6)
  else
   false
 if var_7 then
  let (var_8: char) = var_2.[int32 var_4]
  let (var_10: bool) =
   if (var_8 = ' ') then
    true
   else
    if (var_8 = '\n') then
     true
    else
     (var_8 = '\r')
  let (var_11: int64) = (var_4 + 1L)
  if var_10 then
   method_27((var_0: int64), (var_1: int64), (var_2: string), (var_5: int64), (var_11: int64))
  else
   let (var_12: bool) = (var_1 >= 0L)
   if var_12 then
    let (var_14: ((Union0 []) [])) = Array.zeroCreate<(Union0 [])> (System.Convert.ToInt32(var_1))
    let (var_15: int64) = 0L
    method_29((var_14: ((Union0 []) [])), (var_1: int64), (var_2: string), (var_15: int64), (var_12: bool), (var_4: int64))
   else
    (failwith "n in parse array must be >= 0")
 else
  let (var_16: bool) = (var_1 >= 0L)
  if var_16 then
   let (var_17: ((Union0 []) [])) = Array.zeroCreate<(Union0 [])> (System.Convert.ToInt32(var_1))
   let (var_18: int64) = 0L
   method_29((var_17: ((Union0 []) [])), (var_1: int64), (var_2: string), (var_18: int64), (var_16: bool), (var_4: int64))
  else
   (failwith "n in parse array must be >= 0")
and method_29((var_0: ((Union0 []) [])), (var_1: int64), (var_2: string), (var_3: int64), (var_4: bool), (var_5: int64)): unit =
 let (var_6: bool) = (var_3 < var_1)
 if var_6 then
  if var_4 then
   let (var_7: (Union0 [])) = Array.zeroCreate<Union0> (System.Convert.ToInt32(var_1))
   let (var_8: int64) = 0L
   method_30((var_7: (Union0 [])), (var_0: ((Union0 []) [])), (var_3: int64), (var_1: int64), (var_4: bool), (var_2: string), (var_8: int64), (var_5: int64))
  else
   (failwith "n in parse array must be >= 0")
 else
  let (var_9: int64) = 0L
  method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_9: int64))
and method_30((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_6: int64), (var_7: int64)): unit =
 let (var_8: bool) = (var_6 < var_3)
 if var_8 then
  let (var_10: bool) =
   if (var_7 >= 0L) then
    let (var_9: int64) = (int64 var_5.Length)
    (var_7 < var_9)
   else
    false
  if var_10 then
   let (var_11: char) = var_5.[int32 var_7]
   let (var_12: bool) = ('-' = var_11)
   let (var_13: int64) = (var_7 + 1L)
   if var_12 then
    var_0.[int32 var_6] <- Union0Case0
    let (var_14: int64) = (var_6 + 1L)
    method_30((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_14: int64), (var_13: int64))
   else
    let (var_16: bool) =
     if (var_7 >= 0L) then
      let (var_15: int64) = (int64 var_5.Length)
      (var_7 < var_15)
     else
      false
    if var_16 then
     let (var_17: bool) = ('p' = var_11)
     if var_17 then
      var_0.[int32 var_6] <- Union0Case2
      let (var_18: int64) = (var_6 + 1L)
      method_30((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_18: int64), (var_13: int64))
     else
      let (var_20: bool) =
       if (var_7 >= 0L) then
        let (var_19: int64) = (int64 var_5.Length)
        (var_7 < var_19)
       else
        false
      if var_20 then
       let (var_21: bool) = ('m' = var_11)
       if var_21 then
        var_0.[int32 var_6] <- Union0Case1
        let (var_22: int64) = (var_6 + 1L)
        method_30((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_22: int64), (var_13: int64))
       else
        (failwith "char")
      else
       (failwith "string index out of bounds")
    else
     let (var_24: bool) =
      if (var_7 >= 0L) then
       let (var_23: int64) = (int64 var_5.Length)
       (var_7 < var_23)
      else
       false
     if var_24 then
      let (var_25: bool) = ('m' = var_11)
      if var_25 then
       var_0.[int32 var_6] <- Union0Case1
       let (var_26: int64) = (var_6 + 1L)
       method_30((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_26: int64), (var_13: int64))
      else
       (failwith "char")
     else
      (failwith "string index out of bounds")
  else
   let (var_28: bool) =
    if (var_7 >= 0L) then
     let (var_27: int64) = (int64 var_5.Length)
     (var_7 < var_27)
    else
     false
   if var_28 then
    let (var_29: char) = var_5.[int32 var_7]
    let (var_30: bool) = ('p' = var_29)
    let (var_31: int64) = (var_7 + 1L)
    if var_30 then
     var_0.[int32 var_6] <- Union0Case2
     let (var_32: int64) = (var_6 + 1L)
     method_30((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_32: int64), (var_31: int64))
    else
     let (var_34: bool) =
      if (var_7 >= 0L) then
       let (var_33: int64) = (int64 var_5.Length)
       (var_7 < var_33)
      else
       false
     if var_34 then
      let (var_35: bool) = ('m' = var_29)
      if var_35 then
       var_0.[int32 var_6] <- Union0Case1
       let (var_36: int64) = (var_6 + 1L)
       method_30((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_36: int64), (var_31: int64))
      else
       (failwith "char")
     else
      (failwith "string index out of bounds")
   else
    let (var_38: bool) =
     if (var_7 >= 0L) then
      let (var_37: int64) = (int64 var_5.Length)
      (var_7 < var_37)
     else
      false
    if var_38 then
     let (var_39: char) = var_5.[int32 var_7]
     let (var_40: bool) = ('m' = var_39)
     let (var_41: int64) = (var_7 + 1L)
     if var_40 then
      var_0.[int32 var_6] <- Union0Case1
      let (var_42: int64) = (var_6 + 1L)
      method_30((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_42: int64), (var_41: int64))
     else
      (failwith "char")
    else
     (failwith "string index out of bounds")
 else
  let (var_43: int64) = 0L
  method_31((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_43: int64), (var_7: int64))
and method_31((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_6: int64), (var_7: int64)): unit =
 let (var_8: int64) = (var_6 + 1L)
 let (var_10: bool) =
  if (var_7 >= 0L) then
   let (var_9: int64) = (int64 var_5.Length)
   (var_7 < var_9)
  else
   false
 if var_10 then
  let (var_11: char) = var_5.[int32 var_7]
  let (var_13: bool) =
   if (var_11 = ' ') then
    true
   else
    if (var_11 = '\n') then
     true
    else
     (var_11 = '\r')
  let (var_14: int64) = (var_7 + 1L)
  if var_13 then
   method_31((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_8: int64), (var_14: int64))
  else
   var_1.[int32 var_2] <- var_0
   let (var_15: int64) = (var_2 + 1L)
   method_29((var_1: ((Union0 []) [])), (var_3: int64), (var_5: string), (var_15: int64), (var_4: bool), (var_7: int64))
 else
  var_1.[int32 var_2] <- var_0
  let (var_16: int64) = (var_2 + 1L)
  method_29((var_1: ((Union0 []) [])), (var_3: int64), (var_5: string), (var_16: int64), (var_4: bool), (var_7: int64))
and method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64)): unit =
 if (var_2 < var_1) then
  let (var_3: int64) = 0L
  method_33((var_0: ((Union0 []) [])), (var_2: int64), (var_1: int64), (var_3: int64))
 else
  (failwith "Current position not found.")
and method_33((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64)): unit =
 if (var_3 < var_2) then
  let (var_4: (Union0 [])) = var_0.[int32 var_1]
  let (var_5: Union0) = var_4.[int32 var_3]
  match var_5 with
  | Union0Case0 ->
   let (var_9: int64) = (var_3 + 1L)
   method_33((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_9: int64))
  | Union0Case1 ->
   let (var_10: int64) = (var_3 + 1L)
   method_34((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_10: int64), (var_3: int64))
  | Union0Case2 ->
   let (var_11: int64) = (var_3 + 1L)
   method_62((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_11: int64), (var_3: int64))
 else
  let (var_12: int64) = (var_1 + 1L)
  method_32((var_0: ((Union0 []) [])), (var_2: int64), (var_12: int64))
and method_34((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
 if (var_3 < var_2) then
  let (var_5: (Union0 [])) = var_0.[int32 var_1]
  let (var_6: Union0) = var_5.[int32 var_3]
  match var_6 with
  | Union0Case0 ->
   let (var_10: int64) = (var_3 + 1L)
   method_34((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_10: int64), (var_4: int64))
  | Union0Case1 ->
   let (var_11: int64) = (var_3 + 1L)
   method_34((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_11: int64), (var_3: int64))
  | Union0Case2 ->
   method_38((var_1: int64), (var_4: int64), (var_2: int64), (var_3: int64))
 else
  let (var_12: int64) = (var_1 + 1L)
  method_35((var_0: ((Union0 []) [])), (var_2: int64), (var_12: int64), (var_1: int64), (var_4: int64))
and method_35((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
 if (var_2 < var_1) then
  let (var_5: int64) = 0L
  method_36((var_0: ((Union0 []) [])), (var_2: int64), (var_1: int64), (var_3: int64), (var_4: int64), (var_5: int64))
 else
  (failwith "Current position not found.")
and method_36((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
 if (var_5 < var_2) then
  let (var_6: (Union0 [])) = var_0.[int32 var_1]
  let (var_7: Union0) = var_6.[int32 var_5]
  match var_7 with
  | Union0Case0 ->
   let (var_11: int64) = (var_5 + 1L)
   method_36((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_11: int64))
  | Union0Case1 ->
   let (var_12: int64) = (var_5 + 1L)
   method_37((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_12: int64), (var_5: int64))
  | Union0Case2 ->
   method_59((var_3: int64), (var_4: int64), (var_2: int64), (var_5: int64), (var_1: int64))
 else
  let (var_13: int64) = (var_1 + 1L)
  method_35((var_0: ((Union0 []) [])), (var_2: int64), (var_13: int64), (var_3: int64), (var_4: int64))
and method_37((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64)): unit =
 if (var_5 < var_2) then
  let (var_7: (Union0 [])) = var_0.[int32 var_1]
  let (var_8: Union0) = var_7.[int32 var_5]
  match var_8 with
  | Union0Case0 ->
   let (var_12: int64) = (var_5 + 1L)
   method_37((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_12: int64), (var_6: int64))
  | Union0Case1 ->
   let (var_13: int64) = (var_5 + 1L)
   method_37((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_13: int64), (var_5: int64))
  | Union0Case2 ->
   method_38((var_1: int64), (var_6: int64), (var_2: int64), (var_5: int64))
 else
  let (var_14: int64) = (var_1 + 1L)
  method_35((var_0: ((Union0 []) [])), (var_2: int64), (var_14: int64), (var_1: int64), (var_6: int64))
and method_38((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64)): unit =
 let (var_4: int64) = (var_2 * 1L)
 let (var_5: int64) = (var_2 * var_4)
 let (var_6: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_5))
 let (var_7: int64) = 0L
 let (var_8: int64) = 0L
 let (var_9: int64) = method_40((var_4: int64), (var_2: int64), (var_6: (bool [])), (var_8: int64), (var_7: int64))
 let (var_10: bool) =
  if (var_0 >= 0L) then
   (var_0 < var_2)
  else
   false
 if (var_10 = false) then
  (failwith "Argument out of bounds.")
 else
  ()
 let (var_11: int64) = (var_4 * var_0)
 let (var_12: int64) = (0L + var_11)
 let (var_13: bool) =
  if (var_1 >= 0L) then
   (var_1 < var_2)
  else
   false
 if (var_13 = false) then
  (failwith "Argument out of bounds.")
 else
  ()
 let (var_14: int64) = (1L * var_1)
 let (var_15: int64) = (var_12 + var_14)
 var_6.[int32 var_15] <- true
 let (var_16: string) = "UP"
 let (var_17: string) = "DOWN"
 let (var_18: string) = "LEFT"
 let (var_19: string) = "RIGHT"
 let (var_20: (Tuple1 [])) = Array.zeroCreate<Tuple1> (System.Convert.ToInt32(16L))
 let (var_21: (Env2 ref)) = (ref (Env2(var_20, 0L, 0L)))
 let (var_22: Rec3) = Rec3Case0
 method_47((var_21: (Env2 ref)), (var_0: int64), (var_1: int64), (var_22: Rec3))
 method_51((var_17: string), (var_6: (bool [])), (var_4: int64), (var_2: int64), (var_3: int64), (var_0: int64), (var_21: (Env2 ref)), (var_18: string), (var_19: string), (var_16: string))
and method_40((var_0: int64), (var_1: int64), (var_2: (bool [])), (var_3: int64), (var_4: int64)): int64 =
 if (var_3 < var_1) then
  let (var_5: int64) = 0L
  let (var_6: int64) = method_41((var_3: int64), (var_2: (bool [])), (var_1: int64), (var_5: int64), (var_4: int64))
  let (var_7: int64) = (var_4 + var_0)
  let (var_8: int64) = (var_3 + 1L)
  method_40((var_0: int64), (var_1: int64), (var_2: (bool [])), (var_8: int64), (var_7: int64))
 else
  var_4
and method_41((var_0: int64), (var_1: (bool [])), (var_2: int64), (var_3: int64), (var_4: int64)): int64 =
 if (var_3 < var_2) then
  var_1.[int32 var_4] <- false
  let (var_5: int64) = (var_4 + 1L)
  let (var_6: int64) = (var_3 + 1L)
  method_41((var_0: int64), (var_1: (bool [])), (var_2: int64), (var_6: int64), (var_5: int64))
 else
  var_4
and method_47((var_0: (Env2 ref)), (var_1: int64), (var_2: int64), (var_3: Rec3)): unit =
 let (var_4: Env2) = (!var_0)
 let (var_5: (Tuple1 [])) = var_4.mem_ar
 let (var_6: int64) = var_4.mem_from
 let (var_7: int64) = var_4.mem_to
 var_5.[int32 var_7] <- Tuple1(Tuple4(var_1, var_2), var_3)
 let (var_8: int64) = var_5.LongLength
 let (var_9: int64) = (var_7 + 1L)
 let (var_10: int64) =
  if (var_9 = var_8) then
   0L
  else
   var_9
 let (var_16: Env2) =
  if (var_6 = var_10) then
   let (var_11: int64) = (var_8 * 3L)
   let (var_12: int64) = (var_11 / 2L)
   let (var_13: int64) = (var_12 + 3L)
   let (var_14: (Tuple1 [])) = Array.zeroCreate<Tuple1> (System.Convert.ToInt32(var_13))
   method_48((var_5: (Tuple1 [])), (var_14: (Tuple1 [])), (var_6: int64), (var_8: int64))
   let (var_15: int64) = 0L
   method_50((var_5: (Tuple1 [])), (var_14: (Tuple1 [])), (var_6: int64), (var_8: int64), (var_15: int64))
   (Env2(var_14, 0L, var_8))
  else
   (Env2(var_5, var_6, var_10))
 var_0 := var_16
and method_48((var_0: (Tuple1 [])), (var_1: (Tuple1 [])), (var_2: int64), (var_3: int64)): unit =
 if (var_2 < var_3) then
  let (var_4: Tuple1) = var_0.[int32 var_2]
  var_1.[int32 (var_2 - var_2)] <- var_4
  let (var_5: int64) = (var_2 + 1L)
  method_49((var_0: (Tuple1 [])), (var_1: (Tuple1 [])), (var_2: int64), (var_3: int64), (var_5: int64))
 else
  ()
and method_49((var_0: (Tuple1 [])), (var_1: (Tuple1 [])), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
 if (var_4 < var_3) then
  let (var_5: Tuple1) = var_0.[int32 var_4]
  var_1.[int32 (var_4 - var_2)] <- var_5
  let (var_6: int64) = (var_4 + 1L)
  method_49((var_0: (Tuple1 [])), (var_1: (Tuple1 [])), (var_2: int64), (var_3: int64), (var_6: int64))
 else
  ()
and method_50((var_0: (Tuple1 [])), (var_1: (Tuple1 [])), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
 if (var_4 < var_2) then
  let (var_5: int64) = (var_3 - var_2)
  let (var_6: Tuple1) = var_0.[int32 var_4]
  var_1.[int32 (var_5 + var_4)] <- var_6
  let (var_7: int64) = (var_4 + 1L)
  method_50((var_0: (Tuple1 [])), (var_1: (Tuple1 [])), (var_2: int64), (var_3: int64), (var_7: int64))
 else
  ()
and method_51((var_0: string), (var_1: (bool [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: (Env2 ref)), (var_7: string), (var_8: string), (var_9: string)): unit =
 let (var_10: Tuple1) = method_52((var_6: (Env2 ref)))
 let (var_11: Tuple4) = var_10.mem_0
 let (var_12: Rec3) = var_10.mem_1
 method_53((var_1: (bool [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_9: string), (var_6: (Env2 ref)), (var_0: string), (var_7: string), (var_8: string), (var_11: Tuple4), (var_12: Rec3))
and method_52((var_0: (Env2 ref))): Tuple1 =
 let (var_1: Env2) = (!var_0)
 let (var_2: (Tuple1 [])) = var_1.mem_ar
 let (var_3: int64) = var_1.mem_from
 let (var_4: int64) = var_1.mem_to
 let (var_5: bool) = (var_3 <> var_4)
 if (var_5 = false) then
  (failwith "Cannot dequeue past the end of the queue.")
 else
  ()
 let (var_6: int64) = var_2.LongLength
 let (var_7: int64) = (var_3 + 1L)
 let (var_8: int64) =
  if (var_7 = var_6) then
   0L
  else
   var_7
 var_0 := (Env2(var_2, var_8, var_4))
 var_2.[int32 var_3]
and method_53((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: (Env2 ref)), (var_7: string), (var_8: string), (var_9: string), (var_10: Tuple4), (var_11: Rec3)): unit =
 let (var_12: int64) = var_10.mem_0
 let (var_13: int64) = var_10.mem_1
 let (var_14: int64) = (var_12 - 1L)
 let (var_15: bool) =
  if (var_14 >= 0L) then
   (var_14 < var_2)
  else
   false
 let (var_17: bool) =
  if var_15 then
   if (var_13 >= 0L) then
    (var_13 < var_2)
   else
    false
  else
   false
 let (var_25: bool) =
  if var_17 then
   let (var_18: bool) =
    if (var_14 >= 0L) then
     (var_14 < var_2)
    else
     false
   if (var_18 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_19: int64) = (var_1 * var_14)
   let (var_20: int64) = (0L + var_19)
   let (var_21: bool) =
    if (var_13 >= 0L) then
     (var_13 < var_2)
    else
     false
   if (var_21 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_22: int64) = (1L * var_13)
   let (var_23: int64) = (var_20 + var_22)
   let (var_24: bool) = var_0.[int32 var_23]
   (var_24 = false)
  else
   false
 if var_25 then
  let (var_26: bool) =
   if (var_14 = var_4) then
    (var_13 = var_3)
   else
    false
  if var_26 then
   method_54((var_5: string), (var_11: Rec3))
  else
   let (var_27: bool) =
    if (var_14 >= 0L) then
     (var_14 < var_2)
    else
     false
   if (var_27 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_28: int64) = (var_1 * var_14)
   let (var_29: int64) = (0L + var_28)
   let (var_30: bool) =
    if (var_13 >= 0L) then
     (var_13 < var_2)
    else
     false
   if (var_30 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_31: int64) = (1L * var_13)
   let (var_32: int64) = (var_29 + var_31)
   var_0.[int32 var_32] <- true
   let (var_33: Rec3) = (Rec3Case1(Tuple5(var_5, var_11)))
   method_47((var_6: (Env2 ref)), (var_14: int64), (var_13: int64), (var_33: Rec3))
   method_56((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_7: string), (var_6: (Env2 ref)), (var_8: string), (var_9: string), (var_5: string), (var_10: Tuple4), (var_11: Rec3))
 else
  method_56((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_7: string), (var_6: (Env2 ref)), (var_8: string), (var_9: string), (var_5: string), (var_10: Tuple4), (var_11: Rec3))
and method_54((var_0: string), (var_1: Rec3)): unit =
 match var_1 with
 | Rec3Case0 ->
  System.Console.WriteLine(var_0)
 | Rec3Case1(var_3) ->
  let (var_4: string) = var_3.mem_0
  let (var_5: Rec3) = var_3.mem_1
  method_55((var_4: string), (var_5: Rec3))
and method_55((var_0: string), (var_1: Rec3)): unit =
 match var_1 with
 | Rec3Case0 ->
  System.Console.WriteLine(var_0)
 | Rec3Case1(var_3) ->
  let (var_4: string) = var_3.mem_0
  let (var_5: Rec3) = var_3.mem_1
  method_55((var_4: string), (var_5: Rec3))
and method_56((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: (Env2 ref)), (var_7: string), (var_8: string), (var_9: string), (var_10: Tuple4), (var_11: Rec3)): unit =
 let (var_12: int64) = var_10.mem_0
 let (var_13: int64) = var_10.mem_1
 let (var_14: int64) = (var_12 + 1L)
 let (var_15: bool) =
  if (var_14 >= 0L) then
   (var_14 < var_2)
  else
   false
 let (var_17: bool) =
  if var_15 then
   if (var_13 >= 0L) then
    (var_13 < var_2)
   else
    false
  else
   false
 let (var_25: bool) =
  if var_17 then
   let (var_18: bool) =
    if (var_14 >= 0L) then
     (var_14 < var_2)
    else
     false
   if (var_18 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_19: int64) = (var_1 * var_14)
   let (var_20: int64) = (0L + var_19)
   let (var_21: bool) =
    if (var_13 >= 0L) then
     (var_13 < var_2)
    else
     false
   if (var_21 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_22: int64) = (1L * var_13)
   let (var_23: int64) = (var_20 + var_22)
   let (var_24: bool) = var_0.[int32 var_23]
   (var_24 = false)
  else
   false
 if var_25 then
  let (var_26: bool) =
   if (var_14 = var_4) then
    (var_13 = var_3)
   else
    false
  if var_26 then
   method_54((var_5: string), (var_11: Rec3))
  else
   let (var_27: bool) =
    if (var_14 >= 0L) then
     (var_14 < var_2)
    else
     false
   if (var_27 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_28: int64) = (var_1 * var_14)
   let (var_29: int64) = (0L + var_28)
   let (var_30: bool) =
    if (var_13 >= 0L) then
     (var_13 < var_2)
    else
     false
   if (var_30 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_31: int64) = (1L * var_13)
   let (var_32: int64) = (var_29 + var_31)
   var_0.[int32 var_32] <- true
   let (var_33: Rec3) = (Rec3Case1(Tuple5(var_5, var_11)))
   method_47((var_6: (Env2 ref)), (var_14: int64), (var_13: int64), (var_33: Rec3))
   method_57((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_7: string), (var_6: (Env2 ref)), (var_8: string), (var_5: string), (var_9: string), (var_10: Tuple4), (var_11: Rec3))
 else
  method_57((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_7: string), (var_6: (Env2 ref)), (var_8: string), (var_5: string), (var_9: string), (var_10: Tuple4), (var_11: Rec3))
and method_57((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: (Env2 ref)), (var_7: string), (var_8: string), (var_9: string), (var_10: Tuple4), (var_11: Rec3)): unit =
 let (var_12: int64) = var_10.mem_0
 let (var_13: int64) = var_10.mem_1
 let (var_14: int64) = (var_13 - 1L)
 let (var_15: bool) =
  if (var_12 >= 0L) then
   (var_12 < var_2)
  else
   false
 let (var_17: bool) =
  if var_15 then
   if (var_14 >= 0L) then
    (var_14 < var_2)
   else
    false
  else
   false
 let (var_25: bool) =
  if var_17 then
   let (var_18: bool) =
    if (var_12 >= 0L) then
     (var_12 < var_2)
    else
     false
   if (var_18 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_19: int64) = (var_1 * var_12)
   let (var_20: int64) = (0L + var_19)
   let (var_21: bool) =
    if (var_14 >= 0L) then
     (var_14 < var_2)
    else
     false
   if (var_21 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_22: int64) = (1L * var_14)
   let (var_23: int64) = (var_20 + var_22)
   let (var_24: bool) = var_0.[int32 var_23]
   (var_24 = false)
  else
   false
 if var_25 then
  let (var_26: bool) =
   if (var_12 = var_4) then
    (var_14 = var_3)
   else
    false
  if var_26 then
   method_54((var_5: string), (var_11: Rec3))
  else
   let (var_27: bool) =
    if (var_12 >= 0L) then
     (var_12 < var_2)
    else
     false
   if (var_27 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_28: int64) = (var_1 * var_12)
   let (var_29: int64) = (0L + var_28)
   let (var_30: bool) =
    if (var_14 >= 0L) then
     (var_14 < var_2)
    else
     false
   if (var_30 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_31: int64) = (1L * var_14)
   let (var_32: int64) = (var_29 + var_31)
   var_0.[int32 var_32] <- true
   let (var_33: Rec3) = (Rec3Case1(Tuple5(var_5, var_11)))
   method_47((var_6: (Env2 ref)), (var_12: int64), (var_14: int64), (var_33: Rec3))
   method_58((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_7: string), (var_8: string), (var_6: (Env2 ref)), (var_5: string), (var_9: string), (var_10: Tuple4), (var_11: Rec3))
 else
  method_58((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_7: string), (var_8: string), (var_6: (Env2 ref)), (var_5: string), (var_9: string), (var_10: Tuple4), (var_11: Rec3))
and method_58((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: string), (var_7: (Env2 ref)), (var_8: string), (var_9: string), (var_10: Tuple4), (var_11: Rec3)): unit =
 let (var_12: int64) = var_10.mem_0
 let (var_13: int64) = var_10.mem_1
 let (var_14: int64) = (var_13 + 1L)
 let (var_15: bool) =
  if (var_12 >= 0L) then
   (var_12 < var_2)
  else
   false
 let (var_17: bool) =
  if var_15 then
   if (var_14 >= 0L) then
    (var_14 < var_2)
   else
    false
  else
   false
 let (var_25: bool) =
  if var_17 then
   let (var_18: bool) =
    if (var_12 >= 0L) then
     (var_12 < var_2)
    else
     false
   if (var_18 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_19: int64) = (var_1 * var_12)
   let (var_20: int64) = (0L + var_19)
   let (var_21: bool) =
    if (var_14 >= 0L) then
     (var_14 < var_2)
    else
     false
   if (var_21 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_22: int64) = (1L * var_14)
   let (var_23: int64) = (var_20 + var_22)
   let (var_24: bool) = var_0.[int32 var_23]
   (var_24 = false)
  else
   false
 if var_25 then
  let (var_26: bool) =
   if (var_12 = var_4) then
    (var_14 = var_3)
   else
    false
  if var_26 then
   method_54((var_5: string), (var_11: Rec3))
  else
   let (var_27: bool) =
    if (var_12 >= 0L) then
     (var_12 < var_2)
    else
     false
   if (var_27 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_28: int64) = (var_1 * var_12)
   let (var_29: int64) = (0L + var_28)
   let (var_30: bool) =
    if (var_14 >= 0L) then
     (var_14 < var_2)
    else
     false
   if (var_30 = false) then
    (failwith "Argument out of bounds.")
   else
    ()
   let (var_31: int64) = (1L * var_14)
   let (var_32: int64) = (var_29 + var_31)
   var_0.[int32 var_32] <- true
   let (var_33: Rec3) = (Rec3Case1(Tuple5(var_5, var_11)))
   method_47((var_7: (Env2 ref)), (var_12: int64), (var_14: int64), (var_33: Rec3))
   method_51((var_6: string), (var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_7: (Env2 ref)), (var_8: string), (var_5: string), (var_9: string))
 else
  method_51((var_6: string), (var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_7: (Env2 ref)), (var_8: string), (var_5: string), (var_9: string))
and method_59((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
 let (var_5: int64) = (var_2 * 1L)
 let (var_6: int64) = (var_2 * var_5)
 let (var_7: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_6))
 let (var_8: int64) = 0L
 let (var_9: int64) = 0L
 let (var_10: int64) = method_40((var_5: int64), (var_2: int64), (var_7: (bool [])), (var_9: int64), (var_8: int64))
 let (var_11: bool) =
  if (var_0 >= 0L) then
   (var_0 < var_2)
  else
   false
 if (var_11 = false) then
  (failwith "Argument out of bounds.")
 else
  ()
 let (var_12: int64) = (var_5 * var_0)
 let (var_13: int64) = (0L + var_12)
 let (var_14: bool) =
  if (var_1 >= 0L) then
   (var_1 < var_2)
  else
   false
 if (var_14 = false) then
  (failwith "Argument out of bounds.")
 else
  ()
 let (var_15: int64) = (1L * var_1)
 let (var_16: int64) = (var_13 + var_15)
 var_7.[int32 var_16] <- true
 let (var_17: string) = "UP"
 let (var_18: string) = "DOWN"
 let (var_19: string) = "LEFT"
 let (var_20: string) = "RIGHT"
 let (var_21: (Tuple1 [])) = Array.zeroCreate<Tuple1> (System.Convert.ToInt32(16L))
 let (var_22: (Env2 ref)) = (ref (Env2(var_21, 0L, 0L)))
 let (var_23: Rec3) = Rec3Case0
 method_47((var_22: (Env2 ref)), (var_0: int64), (var_1: int64), (var_23: Rec3))
 method_51((var_18: string), (var_7: (bool [])), (var_5: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_22: (Env2 ref)), (var_19: string), (var_20: string), (var_17: string))
and method_62((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
 if (var_3 < var_2) then
  let (var_5: (Union0 [])) = var_0.[int32 var_1]
  let (var_6: Union0) = var_5.[int32 var_3]
  match var_6 with
  | Union0Case0 ->
   let (var_10: int64) = (var_3 + 1L)
   method_62((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_10: int64), (var_4: int64))
  | Union0Case1 ->
   method_38((var_1: int64), (var_3: int64), (var_2: int64), (var_4: int64))
  | Union0Case2 ->
   let (var_11: int64) = (var_3 + 1L)
   method_62((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_11: int64), (var_3: int64))
 else
  let (var_12: int64) = (var_1 + 1L)
  method_63((var_0: ((Union0 []) [])), (var_2: int64), (var_12: int64), (var_1: int64), (var_4: int64))
and method_63((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
 if (var_2 < var_1) then
  let (var_5: int64) = 0L
  method_64((var_0: ((Union0 []) [])), (var_2: int64), (var_1: int64), (var_3: int64), (var_4: int64), (var_5: int64))
 else
  (failwith "Current position not found.")
and method_64((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
 if (var_5 < var_2) then
  let (var_6: (Union0 [])) = var_0.[int32 var_1]
  let (var_7: Union0) = var_6.[int32 var_5]
  match var_7 with
  | Union0Case0 ->
   let (var_11: int64) = (var_5 + 1L)
   method_64((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_11: int64))
  | Union0Case1 ->
   method_59((var_1: int64), (var_5: int64), (var_2: int64), (var_4: int64), (var_3: int64))
  | Union0Case2 ->
   let (var_12: int64) = (var_5 + 1L)
   method_65((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_12: int64), (var_5: int64))
 else
  let (var_13: int64) = (var_1 + 1L)
  method_63((var_0: ((Union0 []) [])), (var_2: int64), (var_13: int64), (var_3: int64), (var_4: int64))
and method_65((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64)): unit =
 if (var_5 < var_2) then
  let (var_7: (Union0 [])) = var_0.[int32 var_1]
  let (var_8: Union0) = var_7.[int32 var_5]
  match var_8 with
  | Union0Case0 ->
   let (var_12: int64) = (var_5 + 1L)
   method_65((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_12: int64), (var_6: int64))
  | Union0Case1 ->
   method_38((var_1: int64), (var_5: int64), (var_2: int64), (var_6: int64))
  | Union0Case2 ->
   let (var_13: int64) = (var_5 + 1L)
   method_65((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_13: int64), (var_5: int64))
 else
  let (var_14: int64) = (var_1 + 1L)
  method_63((var_0: ((Union0 []) [])), (var_2: int64), (var_14: int64), (var_1: int64), (var_6: int64))
let (var_0: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_1: System.IO.StreamReader) = System.IO.StreamReader(var_0)
let (var_2: string) = var_1.ReadToEnd()
let (var_3: int64) = 0L
let (var_5: bool) =
 if (var_3 >= 0L) then
  let (var_4: int64) = (int64 var_2.Length)
  (var_3 < var_4)
 else
  false
if var_5 then
 let (var_6: char) = var_2.[int32 var_3]
 let (var_7: bool) = ('-' = var_6)
 let (var_8: int64) = (var_3 + 1L)
 if var_7 then
  let (var_9: bool) = false
  method_19((var_9: bool), (var_2: string), (var_8: int64))
 else
  let (var_10: bool) = true
  method_19((var_10: bool), (var_2: string), (var_3: int64))
else
 let (var_11: bool) = true
 method_19((var_11: bool), (var_2: string), (var_3: int64))

