type Player = Serving | Opponent
type Point = Love | Fifteen | Thrity
type Score =
    | Points of Point * Point
    | Forty of Player * Point
    | Deuce
    | Advantage of Player
    | Game of Player

let private addPointToServingPlayer pointServingPlayer pointOpponent = 
    match pointServingPlayer with
    | Love -> Points (Fifteen, pointOpponent)
    | Fifteen -> Points (Thrity, pointOpponent)
    | Thrity -> Forty (Serving, pointOpponent)

let private addPointToOpponentPlayer pointServingPlayer pointOpponent = 
    match pointOpponent with
    | Love -> Points (pointServingPlayer, Fifteen)
    | Fifteen -> Points (pointServingPlayer, Thrity)
    | Thrity -> Forty (Opponent, pointServingPlayer)

let private addPointToOtherPlayer fortyPlayer otherPlayerPoint = 
    match otherPlayerPoint with
    | Love -> Forty (fortyPlayer, Fifteen)
    | Fifteen -> Forty (fortyPlayer, Thrity)
    | Thrity -> Deuce

let computeScore (currentScore: Score) (pointTo: Player) =
    match currentScore, pointTo with
    | Points (pointServingPlayer, pointOpponent), Serving -> 
        addPointToServingPlayer pointServingPlayer pointOpponent
    | Points (pointServingPlayer, pointOpponent), Opponent ->  
        addPointToOpponentPlayer pointServingPlayer pointOpponent
    | Forty (player, _), _ when player = pointTo -> Game player
    | Forty (player, otherPlayerPoints), _ -> 
        addPointToOtherPlayer player otherPlayerPoints
    | Deuce, _ -> Advantage pointTo
    | Advantage player, _ when player = pointTo -> Game player
    | Advantage _, _ -> Deuce
    | Game player, _ -> Game player

// Tests
let expect (expected: Score) (result: Score) =
    if expected <> result
    then failwith $"Expected: %A{expected}; Received: %A{result}"

computeScore (Points (Love, Love)) Opponent |> expect (Points (Love, Fifteen))
computeScore (Points (Fifteen, Fifteen)) Serving |> expect (Points (Thrity, Fifteen))
computeScore (Points (Thrity, Thrity)) Serving |> expect (Forty (Serving, Thrity))
computeScore (Forty (Serving, Love)) Serving |> expect (Game Serving)
computeScore (Forty (Opponent, Thrity)) Opponent |> expect (Game Opponent)
computeScore (Forty (Serving, Thrity)) Opponent |> expect Deuce
computeScore (Forty (Opponent, Thrity)) Serving |> expect Deuce
computeScore Deuce Serving |> expect (Advantage Serving)
computeScore Deuce Opponent |> expect (Advantage Opponent)
computeScore (Advantage Serving) Serving |> expect (Game Serving)
computeScore (Advantage Opponent) Opponent |> expect (Game Opponent)
computeScore (Advantage Serving) Opponent |> expect Deuce
computeScore (Advantage Opponent) Serving |> expect Deuce
computeScore (Game Serving) Serving |> expect (Game Serving)
computeScore (Game Opponent) Opponent |> expect (Game Opponent)
computeScore (Game Serving) Opponent |> expect (Game Serving)
computeScore (Game Opponent) Serving |> expect (Game Opponent)
