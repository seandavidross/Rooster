module Rooster.Card

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

// PipRank * NaturalSuit -> CardIndex
let pip =
    PipCardIndex


// CourtRank * WildSuit -> CardIndex
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


type Card = 
    private NaturalCard of PipIndex
    | CourtCard of CourtIndex * AsRoleIndex


// Some combinations of (Rank, Suit) would make an invalid card;
// the CardIndex type makes it impossible to use those combinations. 

// CardIndex -> Card
let create cardIndex =
    match cardIndex with
    | PipCardIndex pipIndex ->
        NaturalCard pipIndex

    | CourtCardIndex courtIndex ->
        CourtCard (courtIndex, AsNaturalCourt courtIndex)


// Card -> CardIndex * AsRoleIndex
let value card =
    match card with
    | NaturalCard pipIndex ->
        (PipCardIndex pipIndex, AsNaturalPip pipIndex)
    | CourtCard (courtIndex, roleIndex) ->
        (CourtCardIndex courtIndex, roleIndex)


// Card -> Rank
let rank card =
    match card with
    | NaturalCard (r, _) 
    | CourtCard (_, AsNaturalPip (r, _)) ->
        Rank.Pip r
    
    | CourtCard (_, AsNaturalCourt (r, _))
    | CourtCard (_, AsUnnaturalCourt (r, _)) ->
        Rank.Court r


// AsRoleIndex -> Rank
let roleRank role =
    match role with
    | AsNaturalPip (r, _) ->
        Rank.Pip r

    | AsNaturalCourt (r, _)
    | AsUnnaturalCourt (r, _) ->
        Rank.Court r


// Card -> Suit
let suit card =
    match card with
    | NaturalCard (_, s) 
    | CourtCard (_, AsNaturalPip (_, s)) 
    | CourtCard (_, AsUnnaturalCourt (_, s)) ->
        Suit.Natural s
    
    | CourtCard (_, AsNaturalCourt (_, s)) ->
        Suit.Wild s


// Card -> AsRoleIndex
let role card =
    match card with
    | NaturalCard pipIndex
    | CourtCard (_, AsNaturalPip pipIndex) ->
        AsNaturalPip pipIndex
    
    | CourtCard (_, AsNaturalCourt courtIndex) ->
        AsNaturalCourt courtIndex
    
    | CourtCard (_, AsUnnaturalCourt unnaturalIndex) ->
        AsUnnaturalCourt unnaturalIndex


// Card -> AsRoleIndex -> Card option
let wild courtCard roleIndex =
    match courtCard, roleIndex with
    | CourtCard (courtIndex, _), AsNaturalPip _ ->
        Some <| CourtCard (courtIndex, roleIndex)
    
    | CourtCard ((c, s), _), AsUnnaturalCourt (r, _) 
    | CourtCard ((c, s), _), AsNaturalCourt (r, _) when c >= r ->
        Some <| CourtCard ((c, s), roleIndex)
    
    | _ ->
        None


// Card -> Card -> bool
let beats lhs rhs =
    rank lhs > rank rhs


// Card -> Card -> bool
let equals lhs rhs =
    rank lhs = rank rhs


// Card -> Card -> bool
let flush lhs rhs =
    suit lhs = suit rhs


// Card -> Card -> bool
let sort lhs rhs =
    if equals lhs rhs then
        suit lhs < suit rhs
    else
        beats rhs lhs
