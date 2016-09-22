open System

// To start things off, I'll make the zero variance bandit game.

let num_levers = 10
let min_reward = 0
let max_reward = 10

// The optimal strategy for this game would be to keep pulling the lever with the max reward or otherwise to pull each lever
// and then repeat pulling the one with the maximal reward.

let rng = Random()
let make_random_phase (num_levers: int) (min_reward: int) (max_reward: int) =
    Array.init num_levers (fun x -> rng.Next(min_reward,max_reward+1))

let phase = make_random_phase num_levers min_reward max_reward

