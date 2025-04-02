type Player = Serving | Opponent
type Point = Love | Fifteen | Thrity | Forty
type Score =
    | Points of Point * Point
    | Advantage of Player
    | Game of Player

let computeScore (currentScore: Score) (pointTo: Player) =
    let addPoint point = 
        match point with
        | Love -> Fifteen
        | Fifteen -> Thrity
        | Thrity -> Forty

    match currentScore, pointTo with
    | Advantage player, _ when player = pointTo -> Game player
    | Advantage _, _ -> Points (Forty, Forty)
    | Points (Forty, Forty), _ -> Advantage pointTo
    | Points (Forty, _), Serving -> Game Serving
    | Points (_, Forty), Opponent -> Game Opponent
    | Points (pointServingPlayer, pointOpponent), Serving -> 
        Points (addPoint pointServingPlayer, pointOpponent)
    | Points (pointServingPlayer, pointOpponent), Opponent -> 
        Points (pointServingPlayer, addPoint pointOpponent)
    | Game player, _ -> Game player

// Tests
let expect (expected: Score) (result: Score) =
    if expected <> result
    then failwith $"Expected: %A{expected}; Received: %A{result}"

computeScore (Points (Love, Love)) Opponent |> expect (Points (Love, Fifteen))
computeScore (Points (Fifteen, Fifteen)) Serving |> expect (Points (Thrity, Fifteen))
computeScore (Points (Thrity, Thrity)) Serving |> expect (Points (Forty, Thrity))
computeScore (Points (Forty, Love)) Serving |> expect (Game Serving)
computeScore (Points (Thrity, Forty)) Opponent |> expect (Game Opponent)
computeScore (Points (Forty, Thrity)) Opponent |> expect (Points (Forty, Forty))
computeScore (Points (Thrity, Forty)) Serving |> expect (Points (Forty, Forty))
computeScore (Points (Forty, Forty)) Serving |> expect (Advantage Serving)
computeScore (Points (Forty, Forty)) Opponent |> expect (Advantage Opponent)
computeScore (Advantage Serving) Serving |> expect (Game Serving)
computeScore (Advantage Opponent) Opponent |> expect (Game Opponent)
computeScore (Advantage Serving) Opponent |> expect (Points (Forty, Forty))
computeScore (Advantage Opponent) Serving |> expect (Points (Forty, Forty))
computeScore (Game Serving) Serving |> expect (Game Serving)
computeScore (Game Opponent) Opponent |> expect (Game Opponent)
computeScore (Game Serving) Opponent |> expect (Game Serving)
computeScore (Game Opponent) Serving |> expect (Game Opponent)
