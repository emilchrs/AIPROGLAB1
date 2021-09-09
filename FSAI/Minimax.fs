namespace FSAI

type Game() =
//IsOnBoard
    static member IsOnBoard(x : int, y : int) = 
        0 <= x && x <= 7 && 0 <= y && y <= 7
//OtherTile
    static member OtherTile(tile) = 
        if tile = Black
        then White
        if tile = White
        then Black
        raise (new ArgumentException("tile must have value 1 or 2.") :> System.Exception)


//GetWinner

//CountCorners

//Evaluation

//GetValidMoves

//MakeMove

//Minimax

