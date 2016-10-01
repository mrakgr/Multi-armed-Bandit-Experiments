let rec reshuffle list: List<int> =
  match list with
  | [] -> []
  | head :: tail -> head :: reshuffle (List.rev tail)

reshuffle [1..5]

let reshuffle_array (ar: int[]): int[] =
    let shuffled_ar = Array.zeroCreate ar.Length
    let rec take_from_beginning p b e =
        if b <= e then
            shuffled_ar.[p] <- ar.[b]
            take_from_end (p+1) (b+1) e
    and take_from_end p b e =
        if b <= e then
            shuffled_ar.[p] <- ar.[e]
            take_from_beginning (p+1) b (e-1)
    take_from_beginning 0 0 (ar.Length-1)
    shuffled_ar

reshuffle_array [|1..5|]

let split_into_ascending_subsequences (l: int list) =
    let rec loop_outer acc rest =
        let rec loop_inner acc rest =
            match rest with
            | x :: y :: rest when x < y -> loop_inner (x :: acc) (y :: rest)
            | x :: y :: rest -> x :: acc, y :: rest
            | [x;y] when x < y -> x :: y :: acc, []
            | [x;y] -> x :: acc, [y]
            | [x] -> x :: acc, []
            | [] -> acc, []
        let result, rest = loop_inner [] rest
        let result = List.rev result
        match rest with
        | [] -> List.rev (result :: acc)
        | rest -> loop_outer (result :: acc) rest
    loop_outer [] l

split_into_ascending_subsequences [4;3;5;4;3;2]

let split_into_ascending_subsequences_array (ar: int[]) =
    let result_ar = ResizeArray()
    let rec takeFirst (p: int) =
        if p < ar.Length then
            let intermediate_ar = ResizeArray()
            intermediate_ar.Add ar.[p]
            takeRest (p+1) intermediate_ar
    and takeRest (p: int) (intermediate_ar: ResizeArray<int>) =
        if p < ar.Length then
            let prev = Seq.last intermediate_ar
            let cur = ar.[p]
            if prev < cur then
                intermediate_ar.Add cur
                takeRest (p+1) intermediate_ar
            else
                result_ar.Add <| intermediate_ar.ToArray()
                takeFirst p
        else
            result_ar.Add <| intermediate_ar.ToArray()
    takeFirst 0
    result_ar.ToArray()

split_into_ascending_subsequences_array [|4;5;6;3;5;4;3;7;9;2|]

let add (x: int) (y: int) = x + y
let append (x: string) (y: string) = x + y

let ar = ResizeArray()
ar.Add(box add)
ar.Add(append)

for x in ar do
    match x with
    | :? (int -> int -> int) as add -> printfn "2+2=%i" (add 2 2)
    | :? (string -> string -> string) as append -> printfn "Hello%s" (append "Hello" " World!")

type Input<'input,'aux_costs,'aux_inputs,'aux_data> =
    {
    standard_input : 'input
    aux_costs : 'aux_costs
    aux_inputs : 'aux_inputs
    aux_data : 'aux_data
    }

type Output<'output,'aux_costs,'aux_outputs,'aux_data> =
    {
    standard_input : 'output
    aux_costs : 'aux_costs
    aux_inputs : 'aux_outputs
    aux_data : 'aux_data
    }

// Computations that can be run step by step
type Eventually<'T> =
    | Done of 'T
    | NotYetDone of (unit -> Eventually<'T>)

module Eventually =
    // The bind for the computations. Append 'func' to the
    // computation.
    let rec bind func expr =
        match expr with
        | Done value -> NotYetDone (fun () -> func value)
        | NotYetDone work -> NotYetDone (fun () -> bind func (work()))

