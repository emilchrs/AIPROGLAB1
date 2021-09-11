namespace FSAI

type Game() =
//IsOnBoard
    static member IsOnBoard(x : int, y : int) = 
        0 <= x && x <= 7 && 0 <= y && y <= 7
//OtherTile
    static member OtherTile(tile : System.Byte) = 
        if tile = Black
        then White
        if tile = White
        then Black
        raise (new ArgumentException("tile must have value 1 or 2.") :> System.Exception)


//GetWinner

//CountCorners

//Evaluation

//GetValidMoves

static member GetValidMoves(board : byte[*,*], tile : System.Byte) = 
        let mutable (validMoves : List<Tuple<int, int>>) = new List<Tuple<int, int>>()
        do 
            let mutable (X : int) = 0
            while (X < 8) do
                do 
                    let mutable (Y : int) = 0
                    while (Y < 8) do
                        if board.[X, Y] = Empty
                        then 
                            let mutable (doneMove : System.Boolean) = false
                            for (dir : int[]) in dirs do
                                if doneMove
                                then   //break
                                let mutable (x : int) = X + dir.[0]
                                let mutable (y : int) = Y + dir.[1]
                                if IsOnBoard (x, y) && board.[x, y] = OtherTile (tile)
                                then 
                                    x <- x + dir.[0]
                                    y <- y + dir.[1]
                                    while (IsOnBoard (x, y)) do
                                        if board.[x, y] = tile
                                        then 
                                            validMoves.Add (Tuple.Create (X, Y))
                                            doneMove <- true
                                            //break
                                        else 
                                            if board.[x, y] = Empty
                                            then //break
                                        x <- x + dir.[0]
                                        y <- y + dir.[1]
               
        validMoves

//MakeMove

//Minimax

