module Rooster

    type NaturalSuit =
        | Acorns
        | Coins
        | Cups
        | Eggs
        | Feathers
        | Hearts
        | Leaves
        | Swords


    type WildSuit =
        | Upper
        | Lower


    [<RequireQualifiedAccess>]
    type Suit =
        | Natural of NaturalSuit
        | Wild of WildSuit


    type PipRank =
        | One
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven


    type CourtRank =
        | Jack
        | Cavalier
        | King


    [<RequireQualifiedAccess>]
    type Rank =
        | Pip of PipRank
        | Court of CourtRank



    // CARD

    type PipIndex =
        PipRank * NaturalSuit


    type CourtIndex =
        CourtRank * WildSuit

     
    type UnnaturalIndex =
        CourtRank * NaturalSuit


    type CardIndex =
        | Pip of PipIndex
        | Court of CourtIndex


    type AsRoleIndex =
        | AsNaturalPip of PipIndex
        | AsNaturalCourt of CourtIndex
        | AsUnnaturalCourt of UnnaturalIndex


    type WildIndex =
        CourtIndex * AsRoleIndex


    type Card = 
        private NaturalCard of PipIndex
        | CourtCard of CourtIndex
        | WildCard of WildIndex


    module Card =
        let create cardIndex =
            match cardIndex with
                | Pip pipIndex ->
                    Card.NaturalCard pipIndex

                | Court courtIndex ->
                    Card.CourtCard courtIndex

       
        let rank card =
            match card with
                | NaturalCard (r, _) 
                | WildCard (_, AsNaturalPip (r, _)) ->
                    Rank.Pip r
                
                | CourtCard (r, _)
                | WildCard (_, AsNaturalCourt (r, _))
                | WildCard (_, AsUnnaturalCourt (r, _)) ->
                    Rank.Court r
                


        let suit card =
            match card with
                | NaturalCard (_, s) 
                | WildCard (_, AsNaturalPip (_, s)) 
                | WildCard (_, AsUnnaturalCourt (_, s)) ->
                    Suit.Natural s
                
                | CourtCard (_, s)
                | WildCard (_, AsNaturalCourt (_, s)) ->
                    Suit.Wild s

        
        let index card =
            (rank card, suit card)
        

        let rawIndex card =
            match card with
                | NaturalCard (r, s) ->
                    CardIndex.Pip (r, s)
                
                | CourtCard (r, s)
                | WildCard ((r, s), _) ->
                    CardIndex.Court (r, s)

 
        let roleIndex card =
            match card with
                | NaturalCard (r, s) 
                | WildCard (_, AsNaturalPip (r, s)) ->
                    AsNaturalPip (r, s) 
                
                | CourtCard (r, s)
                | WildCard (_, AsNaturalCourt (r, s)) ->
                    AsNaturalCourt (r, s)
                
                | WildCard (_, AsUnnaturalCourt (r, s)) ->
                    AsUnnaturalCourt (r, s)
 
        
        let wild courtCard roleIndex =
            let courtIndex =
                rawIndex courtCard

            match (courtIndex, roleIndex) with
                | (CardIndex.Pip (_), _) ->
                    Error "A pip card cannot be used as a wild card. Use a court card instead."
                | (CardIndex.Court (c, s), AsNaturalPip (r, _)) ->
                    Ok <| WildCard ((c, s), roleIndex)
                
                | (CardIndex.Court (c, s), AsUnnaturalCourt (r, _)) 
                | (CardIndex.Court (c, s), AsNaturalCourt (r, _)) -> 
                    if c >= r then
                        Ok <| WildCard ((c, s), roleIndex)
                    else
                        Error "A wild card cannot assume a rank higher than its own natural rank."


        let beats lhs rhs =
            rank lhs > rank rhs
        

        let equals lhs rhs =
            rank lhs = rank rhs


        let matches lhs rhs =
            suit lhs = suit rhs
        
// TODO: test api. see if more modules and constructors make sense...


    // CONSTRUCTOR CONVENIENCE ALIASES
    let pip = 
        CardIndex.Pip

    
    let court =
        CardIndex.Court

    
    let card =
        Card.create


    let wild =
        Card.wild

    
    let asNaturalPip =
        AsRoleIndex.AsNaturalPip
    

    let asNaturalCourt =
        AsRoleIndex.AsNaturalCourt
    

    let asUnnaturalCourt =
        AsRoleIndex.AsUnnaturalCourt
    


    // SUIT CONVENIENCE ALIASES

    let ACORNS = 
        NaturalSuit.Acorns

     
    let COINS =
        NaturalSuit.Coins


    let CUPS =
        NaturalSuit.Cups


    let EGGS = 
        NaturalSuit.Eggs

     
    let FEATHERS =
        NaturalSuit.Feathers


    let HEARTS =
        NaturalSuit.Hearts


    let LEAVES = 
        NaturalSuit.Leaves


    let SWORDS =
        NaturalSuit.Swords


    let UPPER =
        WildSuit.Upper


    let LOWER =
        WildSuit.Lower



    // RANK CONVENIENCE ALIASES

    let ONE =
        PipRank.One


    let TWO =
        PipRank.Two


    let THREE =
        PipRank.Three


    let FOUR =
        PipRank.Four

     
    let FIVE =
        PipRank.Five


    let SIX =
        PipRank.Six


    let SEVEN =
        PipRank.Seven


    let JACK =
        CourtRank.Jack


    let CAVALIER =
        CourtRank.Cavalier


    let KING =
        CourtRank.King


    let oneOfAcorns = 
        pip (ONE, ACORNS) |> card
    
    let upperJack =
        court (JACK, UPPER) |> card
    
    let wildJack =
        Card.roleIndex oneOfAcorns |> wild upperJack
   
