namespace FSAI

module x =

    type byte =
    | Empty = 0
    | White = 1
    | Black = 2
    | Valid = 3
    | Tie = 4


    let IsOnBoard(x : int, y : int) = 
      0 <= x && x <= 7 && 0 <= y && y <= 7

    let GetScore (board2D: byte[,],t:byte) =  
      0

         
    let GetWinner (board : byte[,]) = 
        let (blackScore : int) = GetScore (board, byte.Black)
        let (whiteScore : int) = GetScore (board, byte.White)
        if blackScore = 0 || whiteScore = 0 || blackScore + whiteScore = 64 || (GetValidMoves (board, Black)).Count + (GetValidMoves (board, White)).Count = 0 then
            if blackScore > whiteScore then byte.Black
            else
                if whiteScore > blackScore then byte.White
                else byte.Tie
        else
        byte.Empty
                
        //OtherTile
    let OtherTile (tile : byte) = 
                if tile = byte.Black
                then byte.White
                elif tile = byte.White
                then byte.Black
                else
                raise (new System.ArgumentException("tile must have value 1 or 2.") :> System.Exception)

    let GetValidMoves(board: byte[,], tile : byte) = 
                       let mutable (validMoves : List<System.Tuple<int, int>>) = new List<Tuple<int, int>>()
                       do 
                           let mutable (X : int) = 0
                           while (X < 8) do
                               do 
                                   let mutable (Y : int) = 0
                                   while (Y < 8) do
                                       if board.[X, Y] = byte.Empty
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
                                                               if board.[x, y] = byte.Empty
                                                               then //break
                                                                   x <- x + dir.[0]
                                                                   y <- y + dir.[1]
                  
                       validMoves           
           
        

    let Evaluation (board : byte[,]) = //gör samma men kollar ej om spelet är slut eller om ingen kan röra sig
              let blackScore = GetScore (board, byte.Black)
              let whiteScore =  GetScore (board, byte.White)
              let bothScore =  blackScore- whiteScore //eval 308

              let moveScoreBlack = GetValidMoves (board, byte.Black) 
              let moveScoreWhite = GetValidMoves (board, byte.White)
              let moveScore = moveScoreBlack.Length - moveScoreWhite.Length //eval 313

              let cornerScoreBlack = countCorners board Black
              let cornerScoreWhite = countCorners board White
              let cornerScore = cornerScoreBlack - cornerScoreWhite // eval 314

              if blackScore = 0 then
                  2000
              elif whiteScore = 0 then
                  -2000
              elif blackScore + whiteScore = 64 || moveScoreBlack.Length + moveScoreWhite.Length = 0 then // elif?
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
    let Max (x : int, y : int) =
               if x > y then x
               else y
    let Min (x : int, y : int) =
        if x < y then x
        else y       
    //Minimax

    let rec MiniMaxAlphaBeta(board : byte[,], depth : int, a : int, b : int, tile : byte, isMaxplayer : System.Boolean) =
            let mutable bestScore = 0 //int
            let mutable x = 0
            let mutable y = 0
            if depth = 0 || GetWinner (board) <> byte.Empty then (Evaluation (board))
            else
                if isMaxplayer
                then bestScore <- -5
                else bestScore <- +5
                let validMoves = (GetValidMoves (board, tile))
                if validMoves.Length > 0
                then
                    for (move : System.Tuple<int, int>) in validMoves do
                        let board2 = board
                        let mutable (childBoard : byte [,]) = Array.copy board
                        MakeMove(childBoard, move, tile)
                        let mutable (nodeScore : int) = MiniMaxAlphaBeta (childBoard, depth-1, a, b, OtherTile(tile), not isMaxplayer)
                        if isMaxplayer
                        then
                            x <- Max (bestScore, a)
                            bestScore <- Max (bestScore, nodeScore)
                            
                        else
                            y <- Min(bestScore, b)
                            bestScore <- Min(bestScore, nodeScore)
                            
                        if (y <= x)
                        then bestScore
                else (MiniMaxAlphaBeta(board, depth, a, b, OtherTile(tile), not isMaxplayer))
                bestScore   
