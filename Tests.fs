module Tests.Rooster

open Expecto
open Rooster.Card

module Card = Rooster.Card


let tests =
    testList "Rooster.Card" [
        testList "Rooster.Card.create" [
            testProperty "Cannot create an invalid card" <| fun cardIndex ->
                let card =
                    Card.create cardIndex 
                    
                match Card.value card with
                | (PipCardIndex _, AsNaturalPip _)
                | (CourtCardIndex _, AsNaturalCourt _) ->
                    true
                | _ ->
                    false        
        ]      
    ]

[<EntryPoint>]
let main argv =
    do (runTestsWithArgs defaultConfig argv tests |> ignore)
    0
