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


    // Keeping the natural ranks and suits seperate from the wild/court card 
    // ranks and suits helps makes it impossible to construct invalid cards. 
    // E.g., its impossible to make a "One of Upper" or a "Jack of Acorns". 
    type Suit =
        | Natural of NaturalSuit
        | Wild of WildSuit

    type Rank =
        | Pip of PipRank
        | Court of CourtRank


    type PipIndex =
        PipRank * NaturalSuit


    type CourtIndex =
        CourtRank * WildSuit

    
    type CardIndex =
        | PipCardIndex of PipIndex
        | CourtCardIndex of CourtIndex


    // a couple of constructor convenience aliases..
    let pip =
        PipCardIndex

    
    let court =
        CourtCardIndex


    // The "natural" suit for a court card is a WildSuit. When used as a wild 
    // card a court card can assume a NaturalSuit, i.e., the "wild" suit for a 
    // court card can be a NaturalSuit. Calling this index a WildIndex would be
    // confusing, so I've opted for UnnaturalIndex as this is not the "natural" 
    // suit for a court card.
    type UnnaturalIndex =
        CourtRank * NaturalSuit

    
    // When used as a wild card, a court card can "act" as though it has a
    // different rank or suit. These are the possible roles it can play...
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

        // Some combinations of (Rank, Suit) would make an invalid card;
        // the CardIndex type makes it impossible to use those combinations. 
        let create cardIndex =
            match cardIndex with
                | PipCardIndex pipIndex ->
                    NaturalCard pipIndex

                | CourtCardIndex courtIndex ->
                    CourtCard courtIndex

       
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

 
        let role card =
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
            match (courtCard, roleIndex) with
                | (CourtCard courtIndex, AsNaturalPip (r, _)) ->
                    Some <| WildCard (courtIndex, roleIndex)
                
                | (CourtCard (c, s), AsUnnaturalCourt (r, _)) 
                | (CourtCard (c, s), AsNaturalCourt (r, _)) when c >= r ->
                    Some <| WildCard ((c, s), roleIndex)
                
                | otherwise ->
                    None


        let beats lhs rhs =
            rank lhs > rank rhs
        

        let equals lhs rhs =
            rank lhs = rank rhs


        let matches lhs rhs =
            suit lhs = suit rhs




// trying out the API...
    let oneOfAcorns = 
        Card.create <| pip (One, Acorns) 
    
    let upperJack =
        Card.create <| court (Jack, Upper) 


    let wildJack =
        Card.wild upperJack <| Card.role oneOfAcorns
 
 
   
