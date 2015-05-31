open System.Collections.Generic

let goalState = [| 0; 1; 2; 3; 4; 5; 6; 7; 8 |]     // 0 represents the empty position

type SearchNode = { state: int array; depth: int }    // States are represented as arrays

// Simple replacement for a priority queue. For this toy problem, it is enough. 
// In the real world, you should use a priority queue.
type Fringe() = class
    let fringe = Array.zeroCreate<Queue<SearchNode>> 32

    do
        for i = 0 to 31 do fringe.[i] <- new Queue<SearchNode>()
    
    member x.Enqueue priority node = fringe.[priority].Enqueue node
    member x.Dequeue() = 
        let mutable queueNo = 0
        while queueNo < 32 && fringe.[queueNo].Count = 0 do queueNo <- queueNo + 1
        if queueNo = 32 then None else Some(fringe.[queueNo].Dequeue())
end

// Calculates number of misplaced tiles
let heuristic1 state =
    Array.fold2 (fun acc g s -> if g <> s then acc + 1 else acc) 0 goalState state

// Calculates Manhattan block distance
let heuristic2 state =
    Array.fold2 (fun acc g s -> acc + abs(g % 3 - s % 3) + abs(g / 3 - s / 3)) 0 goalState state

// Creates a list of all valid successors of the given state
let successors state =
    let pos0 = Array.findIndex (fun s -> s = 0) state
    let col0 = pos0 % 3
    let validActions = new List<int>()
    if col0 > 0 then validActions.Add(pos0-1)
    if col0 < 2 then validActions.Add(pos0+1)
    if pos0 > 2 then validActions.Add(pos0-3)
    if pos0 < 6 then validActions.Add(pos0+3)
    validActions |> Seq.fold(fun acc a ->
        let newState = Array.copy state
        newState.[pos0] <- newState.[a]
        newState.[a] <- 0
        newState::acc) []

let aStarTreeSearch initialState heuristicFun =
    let fringe = new Fringe()

    let rec aStar() =
        match fringe.Dequeue() with
        | None -> failwith "No solution found."
        | Some(thisNode) -> 
            if thisNode.state = goalState then thisNode.depth
            else
                successors thisNode.state |> List.iter (fun s -> fringe.Enqueue (thisNode.depth + heuristicFun s) { state = s; depth = thisNode.depth + 1 })
                aStar()
    
    fringe.Enqueue (heuristicFun initialState) { state = initialState; depth = 0 }
    aStar()

// Makes a number out of the state for use as a key in the hash table
let makeNumber state =
    Array.fold (fun acc a -> acc*10 + a) 0 state

let aStarGraphSearch initialState heuristicFun =
    let fringe = new Fringe()
    let allNodes = new Dictionary<int, int>()

    let rec aStar() =
        match fringe.Dequeue() with
        | None -> failwith "No solution found."
        | Some(thisNode) -> 
            if thisNode.state = goalState then thisNode.depth
            else
                successors thisNode.state |> List.iter (fun s -> 
                    let bigNumber = makeNumber s
                    let (visited, oldDepth) = allNodes.TryGetValue(bigNumber)
                    if not visited || oldDepth > thisNode.depth then
                        if visited then allNodes.Remove(bigNumber) |> ignore
                        fringe.Enqueue (thisNode.depth + heuristicFun s) { state = s; depth = thisNode.depth + 1 }
                        allNodes.Add(bigNumber,thisNode.depth))
                aStar()
    
    fringe.Enqueue (heuristicFun initialState) { state = initialState; depth = 0 }
    allNodes.Add(makeNumber initialState, 0)
    aStar()

// How many different states of the Eight-Puzzle are exactly 27 steps from the solution (= goal state), 
// where the distance to the solution is the length of the shortest path?
let BFSearch27 initialState =
    let fringe = new Queue<SearchNode>()
    let allNodes = new Dictionary<int, bool>()
    let mutable stateCounter = 0

    let rec doSearch() =
        if fringe.Count = 0 then stateCounter
        else
            let thisNode = fringe.Dequeue()
            successors thisNode.state |> List.iter (fun s -> 
                    let bigNumber = makeNumber s
                    if not (allNodes.ContainsKey(bigNumber)) then
                        if thisNode.depth = 26 then stateCounter <- stateCounter + 1
                        else fringe.Enqueue { state = s; depth = thisNode.depth + 1 }
                        allNodes.Add(bigNumber,true))
            doSearch()
    
    fringe.Enqueue { state = initialState; depth = 0 }
    allNodes.Add(makeNumber initialState, true)
    doSearch()

let pathLen1 = aStarTreeSearch [| 1; 6; 4; 8; 7; 0; 3; 2; 5 |] heuristic2
let pathLen2 = aStarGraphSearch [| 1; 6; 4; 8; 7; 0; 3; 2; 5 |] heuristic2
let countStates = BFSearch27 goalState
