
namespace FSAI

module x =

    type byte =
    | Empty = 0
    | White = 1
    | Black = 2
    | Valid = 3
    | Tie = 4

    let addList (list : List<(int * int)>, element : (int * int)) = element :: list 

    let dirs = [|[|(int (- 1)); 1|]; [|0; 1|]; [|1; 1|]; 
               [|(int (- 1)); 0|];                 [|1; 0|]; 
               [|(int (- 1)); (int (- 1))|]; [|0; (int (- 1))|]; [|1; (int (- 1))|]|]

    let IsOnBoard(x : int, y : int) = 
      0 <= x && x <= 7 && 0 <= y && y <= 7

      
    let GetScore (board2D: byte[,],tile:byte) =  
      let test = Seq.cast board2D 
      test |> Seq.filter (fun x -> x = tile) |> Seq.length
      
      
    let countCorners (board : byte[,], tile : byte) =
        let corner = seq {Array2D.get board 0 0;Array2D.get board 7 0;Array2D.get board 0 7;Array2D.get board 7 7 }
        corner |> Seq.filter (fun x -> x = tile) |> Seq.length
                
        //OtherTile
    let OtherTile (tile : byte) = 
                if tile = byte.Black
                then byte.White
                elif tile = byte.White
                then byte.Black
                else
                raise (new System.ArgumentException("tile must have value 1 or 2.") :> System.Exception)

    let GetValidMoves(board: byte[,], tile : byte) = 
                       let mutable (validMoves : List<int * int>) = List.Empty
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
                                               then  
                                                   let mutable (x : int) = X + dir.[0]
                                                   let mutable (y : int) = Y + dir.[1]
                                                   if IsOnBoard (x, y) && board.[x, y] = OtherTile (tile)
                                                   then 
                                                       x <- x + dir.[0]
                                                       y <- y + dir.[1]
                                                       while (IsOnBoard (x, y)) do
                                                           if board.[x, y] = tile
                                                           then 
                                                               let z = addList(validMoves, (X, Y)) 
                                                               doneMove <- true
                                                               
                                                           else 
                                                               if board.[x, y] = byte.Empty
                                                               then 
                                                                   x <- x + dir.[0]
                                                                   y <- y + dir.[1]
                             
                       validMoves           
           
        
    let GetWinner (board : byte[,]) = 
           let (blackScore : int) = GetScore (board, byte.Black)
           let (whiteScore : int) = GetScore (board, byte.White)
           if blackScore = 0 || whiteScore = 0 || blackScore + whiteScore = 64 || (GetValidMoves (board, byte.Black)).Length + (GetValidMoves (board, byte.White)).Length = 0 then
               if blackScore > whiteScore then byte.Black
               else
                   if whiteScore > blackScore then byte.White
                   else byte.Tie
           else
           byte.Empty


    let Evaluation (board : byte[,]) = //gör samma men kollar ej om spelet är slut eller om ingen kan röra sig
              let blackScore = GetScore (board, byte.Black)
              let whiteScore =  GetScore (board, byte.White)
              let bothScore =  blackScore - whiteScore //eval 308

              let moveScoreBlack = GetValidMoves (board, byte.Black) 
              let moveScoreWhite = GetValidMoves (board, byte.White)
              let moveScore = moveScoreBlack.Length - moveScoreWhite.Length //eval 313

              let cornerScoreBlack = countCorners (board, byte.Black)
              let cornerScoreWhite = countCorners (board, byte.White)
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

   


    let GetFlippedPieces(board : byte[,], move : int * int, tile : byte) =
        let (moveX : int) = fst move
        let (moveY : int) = snd move
        let flippedPieces : List<int * int> = List.Empty  
        if board.[moveX, moveY] = byte.Empty then
            for(dir : int[]) in dirs do
                let dirFlippedPieces : List<int * int> = List.Empty
                let mutable (x : int) = moveX + dir.[0]
                let mutable (y : int) = moveY + dir.[1]
                if IsOnBoard (x,y) && board.[x,y] = OtherTile(tile) then
                    let z = addList(flippedPieces, (x,y))
                    x <- x + dir.[0]
                    y <- y + dir.[1]
                    while (IsOnBoard (x,y)) do
                        if board.[x, y] = tile
                        then 
                            if board.[x,y] = byte.Empty
                            then 
                                x <- x + dir.[0]
                                y <- y + dir.[1]
        flippedPieces


    let MakeMove(board : byte[,], (move : (int * int)), tile : byte) = 
         let flippedPieces = GetFlippedPieces(board,move,tile)
         let board2 = Array2D.copy board
         for flippedPiece in flippedPieces do 
             board2.[fst flippedPiece, snd flippedPiece] <- tile
         if not flippedPieces.IsEmpty then 
            board2.[(fst move), (snd move)] <- tile
         board2


    let Max (x : int, y : int) =
               if x > y then x
               else y
    let Min (x : int, y : int) =
        if x < y then x
        else y       
    //Minimax

    let rec MinMaxAlphaBeta (board, depth, a, b, tile, isMaxPLayer) =
        if (depth = 0 || (GetWinner board  <> byte.Empty)) then
            Evaluation (board)
        else
        let bestScore = match isMaxPLayer with
                        | true -> System.Int32.MinValue
                        | false -> System.Int32.MaxValue

        let rec LoopMoves (board:byte[,], validMoves:List<int*int>, tile:byte, isMaxPlayer:bool, bestScore:int,  a:int, b:int) =
            match validMoves with
            | [] -> bestScore
            | head::tail -> 
                let boardCopy = Array2D.copy board
                let childBoard = MakeMove (boardCopy, head, tile)
                let nodeScore = MinMaxAlphaBeta (childBoard, (depth-1), a, b, (OtherTile tile), (not isMaxPlayer))
                if isMaxPlayer then
                    let topScore = Max (bestScore, nodeScore)
                    let alpha = Max (topScore, a)
                    if (b <= alpha) then
                        topScore
                    else
                       LoopMoves (board, tail, tile, isMaxPlayer, topScore, alpha, b)

                else
                    let topScore = Min (bestScore, nodeScore)
                    let beta = Min (topScore, b)
                    if (beta <= a) then
                        topScore
                    else
                       LoopMoves (board, tail, tile, isMaxPlayer, topScore, a, beta)

                
        
        let validMoves = GetValidMoves (board, tile)

        if (validMoves.IsEmpty) then
            MinMaxAlphaBeta (board, depth, a, b, (OtherTile tile), (not isMaxPLayer))
        else
            LoopMoves (board,validMoves, tile, isMaxPLayer, bestScore, a, b)

