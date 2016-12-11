// For http://adventofcode.com/2016/day/1

let input = 
    "R3, L5, R1, R2, L5, R2, R3, L2, L5, R5, L4, L3, R5, L1, R3, R4, R1, L3, R3, L2, L5, L2, R4, R5, R5, L4, L3, L3, R4, R4, R5, L5, L3, R2, R2, L3, L4, L5, R1, R3, L3, R2, L3, R5, L194, L2, L5, R2, R1, R1, L1, L5, L4, R4, R2, R2, L4, L1, R2, R53, R3, L5, R72, R2, L5, R3, L4, R187, L4, L5, L2, R1, R3, R5, L4, L4, R2, R5, L5, L4, L3, R5, L2, R1, R1, R4, L1, R2, L3, R5, L4, R2, L3, R1, L4, R4, L1, L2, R3, L1, L1, R4, R3, L4, R2, R5, L2, L3, L3, L1, R3, R5, R2, R3, R1, R2, L1, L4, L5, L2, R4, R5, L2, R4, R4, L3, R2, R1, L4, R3, L3, L4, L3, L1, R3, L2, R2, L4, L4, L5, R3, R5, R3, L2, R5, L2, L1, L5, L1, R2, R4, L5, R2, L4, L5, L4, L5, L2, L5, L4, R5, R3, R2, R2, L3, R3, L2, L5"
    |> fun x -> x.Split([|", "|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x.[0], int x.[1..])

let turn d s = 
    match d with
    | 'R' -> (s + 90u) % 360u
    | 'L' -> (s + 270u) % 360u

let go_forward s num_steps (cur_x, cur_y) =
    match s with
    | 0u -> (cur_x, cur_y+num_steps)
    | 90u -> (cur_x+num_steps, cur_y)
    | 180u -> (cur_x, cur_y-num_steps)
    | 270u -> (cur_x-num_steps, cur_y)

let (_,(end_x, end_y)) = 
    Array.fold (fun (state, cur_xy) (lr, num_steps) -> 
        let s = turn lr state
        (s, go_forward s num_steps cur_xy)
        ) (0u,(0,0)) input

let result = abs end_x + abs end_y