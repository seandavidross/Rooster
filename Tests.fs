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


        testList "Rooster.Card.beats" [
            testProperty "Natural cards compare correctly" <| fun lhsIndex rhsIndex ->
                let lhs =
                    Card.create lhsIndex 
                    
                let rhs =
                    Card.create rhsIndex

                match Card.value lhs, Card.value rhs with
                | (PipCardIndex _, _), (CourtCardIndex _, _) ->
                    not <| Card.beats lhs rhs
                | (CourtCardIndex _, _), (PipCardIndex _, _) ->
                    Card.beats lhs rhs
                | (CourtCardIndex (l, _), _), (CourtCardIndex (r, _), _) ->
                    match l, r with
                    | Jack, Jack | Jack, Cavalier | Jack, King
                    | Cavalier, Cavalier | Cavalier, King
                    | King, King ->
                        not <| Card.beats lhs rhs
                    | _ ->
                        Card.beats lhs rhs
                | (PipCardIndex (l, _), _), (PipCardIndex (r, _), _) ->
                    match l, r with
                    | One, One | One, Two | One, Three 
                    | One, Four | One, Five | One, Six | One, Seven
                    | Two, Two | Two, Three | Two, Four
                    | Two, Five | Two, Six | Two, Seven
                    | Three, Three | Three, Four | Three, Five
                    | Three, Six | Three, Seven
                    | Four, Four | Four, Five | Four, Six | Four, Seven
                    | Five, Five | Five, Six | Five, Seven
                    | Six, Six | Six, Seven
                    | Seven, Seven ->
                        not <| Card.beats lhs rhs
                    | _ ->
                        Card.beats lhs rhs    


            testProperty "Pip and Wild cards compare correctly" <| fun lhsIndex rhsIndex ->
                let lhsPipIndex =
                    Card.pip lhsIndex 
                    
                let rhsPipIndex =
                    Card.pip rhsIndex

                let lhs =
                    Card.create lhsPipIndex

                let rhs =
                    Card.create rhsPipIndex

                let jack =
                    Card.create <| Card.court (Jack, Lower)

                let wildJack =
                    Card.wild jack (AsNaturalPip rhsIndex)

                match wildJack with
                | Some rhs' ->
                    Card.beats lhs rhs = Card.beats lhs rhs'    
                | None ->
                    false


            testProperty "Wild cards compare correctly" <| fun lhsIndex rhsIndex lhsRole rhsRole ->
                let lhsCourt =
                    Card.create <| Card.court lhsIndex 
                    
                let rhsCourt =
                    Card.create <| Card.court rhsIndex

                let lhsWild =
                    Card.wild lhsCourt lhsRole

                let rhsWild =
                    Card.wild rhsCourt rhsRole    

                match lhsWild, rhsWild with
                | Some lhs, Some rhs ->
                    match Card.roleRank lhsRole, Card.roleRank rhsRole with
                    | l, r ->
                        Card.beats lhs rhs = (l > r)    
                | _ ->
                    true // ignore invalid wild cards

        ]

    ]

[<EntryPoint>]
let main argv =
    do (runTestsWithArgs defaultConfig argv tests |> ignore)
    0
