open System


// Define types

type Cell =
    | X
    | O
    | Empty
    member self.toString () =
        match self with
        | X -> "X"
        | O -> "O"
        | Empty -> " "

type Board = {
        x0y0: Cell;
        x0y1: Cell;
        x0y2: Cell;
        x1y0: Cell;
        x1y1: Cell;
        x1y2: Cell;
        x2y0: Cell; 
        x2y1: Cell;
        x2y2: Cell;
    }

type Player = Player1 | Player2

type GameState = Player1Turn | Player2Turn | Tie | Win


// Initialize game

let initialState = {
        x0y0 = Empty;
        x0y1 = Empty;
        x0y2 = Empty;
        x1y0 = Empty;
        x1y1 = Empty;
        x1y2 = Empty;
        x2y0 = Empty;
        x2y1 = Empty;
        x2y2 = Empty;
    }

let validInput = ["0"; "1"; "2"]


// Game functions

let checkGameState mark state =
    let playerTurn =
        match mark with
        | X -> Player2Turn
        | O -> Player1Turn
        | _ -> failwith "\nAn unexpected error occurred"

    let isWon =
        match state with
        | x when x.x0y0 = mark && x.x1y0 = mark && x.x2y0 = mark -> Win
        | x when x.x0y1 = mark && x.x1y1 = mark && x.x2y1 = mark -> Win
        | x when x.x0y2 = mark && x.x1y2 = mark && x.x2y2 = mark -> Win
        | x when x.x0y0 = mark && x.x0y1 = mark && x.x0y2 = mark -> Win
        | x when x.x1y0 = mark && x.x1y1 = mark && x.x1y2 = mark -> Win
        | x when x.x2y0 = mark && x.x2y1 = mark && x.x2y2 = mark -> Win
        | x when x.x0y0 = mark && x.x1y1 = mark && x.x2y2 = mark -> Win
        | x when x.x0y2 = mark && x.x1y1 = mark && x.x2y0 = mark -> Win
        | x when x.x0y0 <> Empty && x.x1y0 <> Empty && x.x2y0 <> Empty &&
                 x.x0y1 <> Empty && x.x1y1 <> Empty && x.x2y1 <> Empty &&
                 x.x0y2 <> Empty && x.x1y2 <> Empty && x.x2y2 <> Empty -> Tie
        | _ -> playerTurn
    isWon

let updateGameState state mark position =
    let newState = 
        match position with
        | (0,0) -> { state with x0y0 = mark }
        | (0,1) -> { state with x0y1 = mark }
        | (0,2) -> { state with x0y2 = mark }
        | (1,0) -> { state with x1y0 = mark }
        | (1,1) -> { state with x1y1 = mark }
        | (1,2) -> { state with x1y2 = mark }
        | (2,0) -> { state with x2y0 = mark }
        | (2,1) -> { state with x2y1 = mark }
        | (2,2) -> { state with x2y2 = mark }
        | _ -> failwith "\nAn unexpected error occurred"
    newState

let occupiedCell state position =
    match position with
    | (0,0) -> if state.x0y0 = Empty then true else false
    | (0,1) -> if state.x0y1 = Empty then true else false
    | (0,2) -> if state.x0y2 = Empty then true else false
    | (1,0) -> if state.x1y0 = Empty then true else false
    | (1,1) -> if state.x1y1 = Empty then true else false
    | (1,2) -> if state.x1y2 = Empty then true else false
    | (2,0) -> if state.x2y0 = Empty then true else false
    | (2,1) -> if state.x2y1 = Empty then true else false
    | (2,2) -> if state.x2y2 = Empty then true else false
    | _ -> failwith "\nAn unexpected error occurred"

let rec takeTurn player currentState =
    match player with
    | Player1 -> printfn "\n\nPlayer 1's turn"
    | Player2 -> printfn "\n\nPlayer 2's turn"

    printfn "\nEnter x coordinate:"
    let x = Console.ReadLine()

    printfn "Enter y coordinate:"
    let y = Console.ReadLine()

    let position =
        if List.contains x validInput && List.contains y validInput then
            (x |> int, y |> int)
        else (-1, -1)

    if position = (-1,-1) || not (occupiedCell currentState position) then
        printfn "\nInvalid input, try again."
        takeTurn player currentState
    else
        position

let drawBoard state =
    printfn "\n %s | %s | %s " (state.x0y0.toString()) (state.x1y0.toString()) (state.x2y0.toString())
    printfn "-----------"
    printfn " %s | %s | %s " (state.x0y1.toString()) (state.x1y1.toString()) (state.x2y1.toString())
    printfn "-----------"
    printfn " %s | %s | %s " (state.x0y2.toString()) (state.x1y2.toString()) (state.x2y2.toString())
    
let rec runGame currentState currentPlayer =
    let chosenPosition = takeTurn currentPlayer currentState

    let mark = 
        match currentPlayer with
        | Player1 -> X
        | Player2 -> O

    let newState = updateGameState currentState mark chosenPosition

    drawBoard newState

    let status = checkGameState mark newState

    match status with
    | Player1Turn -> runGame newState Player1
    | Player2Turn -> runGame newState Player2
    | Tie -> "\nThe game ended in a tie!"
    | Win -> match currentPlayer with
             | Player1 -> "\nPlayer 1 is the winner!"
             | Player2 -> "\nPlayer 2 is the winner!"
    

// Run game

[<EntryPoint>]
let main argv = 
    printfn "%s" (runGame initialState Player1)
    0