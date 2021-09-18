
namespace FSAI



module minimax =


    [<Literal>] 
    let Empty = 0uy
    [<Literal>] 
    let White = 1uy
    [<Literal>] 
    let Black = 2uy
    [<Literal>] 
    let Valid = 3uy
    [<Literal>] 
    let Tie = 4uy




    let addList (list : List<(int * int)>, element : (int * int)) = element :: list 
    //adjacent tiles
    let dirs = [|[|(int (- 1)); 1|]; [|0; 1|]; [|1; 1|]; 
               [|(int (- 1)); 0|];                 [|1; 0|]; 
               [|(int (- 1)); (int (- 1))|]; [|0; (int (- 1))|]; [|1; (int (- 1))|]|]
    
    //Kollar så att pjäserna inte kommer utanför brädet som är 8x8 stor
    let IsOnBoard(x : int, y : int) = 
      0 <= x && x <= 7 && 0 <= y && y <= 7

    // returnerar mängen av pjäser som matchar den inskickade pjäsen
    let GetScore (board2D: byte[,],tile:byte) =  
      let test = Seq.cast board2D 
      test |> Seq.filter (fun x -> x = tile) |> Seq.length
      
    // returnerar mängen av pjäser som är i hörn och matchar den inskickade pjäsen  
    //lägger in alla hörner i en sequence och kollar hur många som matchar input
    let countCorners (board : byte[,], tile : byte) =
        let corner = seq {Array2D.get board 0 0;Array2D.get board 7 0;Array2D.get board 0 7;Array2D.get board 7 7 }
        corner |> Seq.filter (fun x -> x = tile) |> Seq.length
                
    // Returnerar motsattpjäs 
    let OtherTile (tile : byte) = 
                if tile = Black
                then White
                elif tile = White
                then Black
                else
                raise (new System.ArgumentException("tile must have value 1 or 2.") :> System.Exception)

    //Kollar vilka moves som går att göra
    let GetValidMoves(board: byte[,], tile : byte) = 
                       let mutable (validMoves : List<int * int>) = List.Empty 
                       do 
                           let mutable (X : int) = 0
                           while (X < 8) do 
                                   let mutable (Y : int) = 0
                                   while (Y < 8) do
                                       if board.[X, Y] = Empty
                                       then 
                                           let mutable (doneMove : System.Boolean) = false
                                           for (dir : int[]) in dirs do  //kollar alla närliggande pjäser
                                               if doneMove //om pjäsen är lagd och inom brädet, 
                                               then  
                                                   let mutable (x : int) = X + dir.[0]
                                                   let mutable (y : int) = Y + dir.[1]
                                                   if IsOnBoard (x, y) && board.[x, y] = OtherTile (tile) //kollar så det är motståndar pjäs
                                                   then 
                                                       x <- x + dir.[0]
                                                       y <- y + dir.[1]
                                                       while (IsOnBoard (x, y)) do
                                                           if board.[x, y] = tile //hittar man sin pjäs gör till valid move
                                                           then 
                                                               let z = addList(validMoves, (X, Y)) 
                                                               doneMove <- true
                                                               
                                                           else 
                                                               if board.[x, y] = Empty
                                                               then 
                                                                   x <- x + dir.[0]
                                                                   y <- y + dir.[1]
                             
                       validMoves           
           
    //Anropar vinnaren av spelet   
    let GetWinner (board : byte[,]) = 
           let (blackScore : int) = GetScore (board, Black) //Anropar getScore får att se hur många pjäser vardera spelare har
           let (whiteScore : int) = GetScore (board, White)
           //Kollar så matchen inte är över/ingen kan göra ett move länger
           if blackScore = 0 || whiteScore = 0 || blackScore + whiteScore = 64 || (GetValidMoves (board, Black)).Length + (GetValidMoves (board, White)).Length = 0 then 
               if blackScore > whiteScore then Black  //returnerar färgen med flest poäng/vinnaren
               else
                   if whiteScore > blackScore then White
                   else Tie
           else
           Empty

           //ger 1 poäng per pjäs, 3 per möjliga moves, 10 per hörn
    let Evaluation (board : byte[,]) = 
              let blackScore = GetScore (board, Black)//hämtar de olika färgernas poäng
              let whiteScore =  GetScore (board, White)
              let bothScore =  blackScore - whiteScore //eval 308

              let moveScoreBlack = GetValidMoves (board, Black) 
              let moveScoreWhite = GetValidMoves (board, White)
              let moveScore = moveScoreBlack.Length - moveScoreWhite.Length //eval 313

              let cornerScoreBlack = countCorners (board, Black)
              let cornerScoreWhite = countCorners (board, White)
              let cornerScore = cornerScoreBlack - cornerScoreWhite // eval 314

              if blackScore = 0 then//om någon inte har några pjäser kvar
                  2000
              elif whiteScore = 0 then
                  -2000
              elif blackScore + whiteScore = 64 || moveScoreBlack.Length + moveScoreWhite.Length = 0 then // kollar så det finns drag att göra
                  if blackScore < whiteScore then
                      (-1000 - whiteScore + blackScore)
                  elif blackScore < whiteScore then
                      (1000 - whiteScore + blackScore)
                  else
                      0
              elif blackScore + whiteScore > 55 then 
                  bothScore
              else 
              (bothScore + 3 * moveScore + 10 * cornerScore) //själva evaluation returnationen


    //MakeMove

   

    //hämtar alla pjäser som ligger mellan sig själv och alla riktningar
    let GetFlippedPieces(board : byte[,], move : int * int, tile : byte) =
        let (moveX : int) = fst move
        let (moveY : int) = snd move
        let flippedPieces : List<int * int> = List.Empty  
        if board.[moveX, moveY] = Empty then
            for(dir : int[]) in dirs do
                let dirFlippedPieces : List<int * int> = List.Empty
                let mutable (x : int) = moveX + dir.[0]
                let mutable (y : int) = moveY + dir.[1]
                if IsOnBoard (x,y) && board.[x,y] = OtherTile(tile) then //Kontrollerar så att det är inom brädet samt kollar om det är motståndarpjäs
                    let z = addList(flippedPieces, (x,y))
                    x <- x + dir.[0]
                    y <- y + dir.[1]
                    while (IsOnBoard (x,y)) do
                        if board.[x, y] = tile
                        then 
                            if board.[x,y] = Empty
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

    let rec MinMaxAlphaBeta (board : byte[,], depth: int, a: int, b: int, tile : byte, isMaxPLayer: bool) =
        if (depth = 0 || (GetWinner board  <> Empty)) then //check if funktion is done or if a win
            Evaluation (board)
        else
        let bestScore = match isMaxPLayer with  //set start value for bestSCore
                        | true -> System.Int32.MinValue
                        | false -> System.Int32.MaxValue

        let rec LoopMoves (board:byte[,], validMoves:List<int*int>, tile:byte, isMaxPlayer:bool, bestScore:int,  a:int, b:int) =
            match validMoves with   //match validmove untill empty to loop all valid moves, likt(for each)
            | [] -> bestScore
            | head::tail -> 
                let boardCopy = Array2D.copy board// gör en kopia av brädet och använder den för nodeScore och göra rörelse
                let childBoard = MakeMove (boardCopy, head, tile)
                let nodeScore = MinMaxAlphaBeta (childBoard, (depth-1), a, b, (OtherTile tile), (not isMaxPlayer))
                if isMaxPlayer then // beroende på spelare kör minMAx med alpha eller beta, Loopa rekursivt för testa flera lösningar
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

                
        //hämta anta moves man kan göra och loopa huvudfunktion om de inte finns några
        let validMoves = GetValidMoves (board, tile)
        if (validMoves.IsEmpty) then
            MinMaxAlphaBeta (board, depth, a, b, (OtherTile tile), (not isMaxPLayer))
        else
            LoopMoves (board,validMoves, tile, isMaxPLayer, bestScore, a, b)

