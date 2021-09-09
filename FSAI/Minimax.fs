namespace FSAI

module Minimax =
    let printHello message = 
        $"Hello, world {message}"

    type Class1() = 
        member this.X = "F#"
    
//OtherTile

type OtherTile = 
    | Black
    | White
    member this.opposite =
        match this with
        | Black -> White
        | White -> Black
//GetWinner

//CountCorners

//Evaluation

//GetValidMoves

//MakeMove

//Minimax

