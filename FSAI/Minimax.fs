namespace FSAI

module x =

    type tile =
    | Black
    | White
    | Empty

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

            let getScore board2D (t:tile) =   //råkade göra "getscore" fast bättre
                let board1D = board2D |>  List.reduce List.append
                let board =  board1D |> List.sumBy (fun x -> 
                    if x = t then 1 
                    else 0)
                board
        //GetWinner
            static member GetWinner(board : byte[,]) = 
                let mutable (blackScore : int) = GetScore (board, Black)
                let mutable (whiteScore : int) = GetScore (board, White)
                if blackScore = 0 || whiteScore = 0 || blackScore + whiteScore = 64 || (GetValidMoves (board, Black)).Count + (GetValidMoves (board, White)).Count = 0
                then
                    if blackScore > whiteScore
                    then Black
                    else
                        if whiteScore > blackScore
                        then White
                        else Tie
                Empty
        //CountCorners
            let countCornes board2D (t:tile)=
                0
            
        //Evaluation

        //GetValidMoves

            static member GetValidMoves(board, tile : System.Byte) = 
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

            let Evaluation board = //gör samma men kollar ej om spelet är slut eller om ingen kan röra sig
                let blackScore = getScore board Black
                let whiteScore =  getScore board White
                let bothScore =  blackScore- whiteScore //eval 308

                let moveScoreBlack = getValidMove Black 
                let moveScoreWhite = getValidMove White
                let moveScore = moveScoreBlack - moveScoreWhite //eval 313

                let cornerScoreBlack = countCorners board Black
                let cornerScoreWhite = countCorners board White
                let cornerScore = cornerScoreBlack - cornerScoreWhite // eval 314

                if blackScore = 0 then
                    2000
                elif whiteScore = 0 then
                    -2000
                elif blackScore + whiteScore = 64 || moveScoreBlack + moveScoreWhite = 0 then // elif?
                    if blackScore < whiteScore then
                        (-1000 - whiteScore + blackScore)
                    elif blackScore < whiteScore then
                         (1000 - whiteScore + blackScore)
                    else
                         0
                elif blackScore + whiteScore > 55 then 
                    bothScore
                else 
                bothScore + 3 * moveScore + 10 * cornerScore

    //MakeMove
            let Max x y =
                if x > y then x
                else y
            let Min x y =
                if x < y then x
                else y
    //Minimax

            let rec MiniMaxAlphaBeta(board : byte, depth : int, a : int, b : int, tile : System.Byte, isMaxplayer : System.Boolean) =
                if depth = 0 || GetWinner (board) <> Empty
                then (Evaluation (board))
                let mutable bestScore = 0 //int
                if isMaxplayer
                then bestScore <- int.MinValue
                else bestScore <- int.MaxValue
                let mutable (validMoves : List<Tuple<int,int>>) = (GetValidMoves (board, tile)) :> List<Tuple>
                if validMoves.Count > 0
                then
                    for (move : Tuple<int, int>) in validMoves do
                        let board2 = board
                        let mutable (childBoard : byte [,]) = Array.copy board
                        MakeMove(childBoard, move, tile)
                        let mutable (nodeScore : int) = MiniMaxAlphaBeta (childBoard, depth-1, a, b, OtherTile(tile), not isMaxplayer)
                        if isMaxplayer
                        then
                            bestScore <- Math.Max(bestScore, nodeScore)
                            a <- Max bestScore, a
                        else
                            bestScore <- Math.Min(bestScore, nodeScore)
                            b <- Min(bestScore, b)
                        if (b <= a)
                        then 0
                else (MiniMaxAlphaBeta(board, depth, a, b, OtherTile(tile), not isMaxplayer))
                bestScore
