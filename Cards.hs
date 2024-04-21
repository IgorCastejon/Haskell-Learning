data Rank 
    = Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two
    deriving (Show, Enum)

data Suit 
    = Clubs
    | Diamonds
    | Hearts
    | Spades
    deriving (Show, Enum)

data Card 
    = Card Suit Rank

instance Show Card where
    show :: Card -> String
    show (Card suit rank) = show rank ++ " of " ++ show suit


data PlayerHand = PlayerHand Card Card

newtype Cards 
    = Cards [Card]
    deriving (Show)

newtype Deck 
    = Deck Cards
    deriving (Show)

standard52Deck :: Deck
standard52Deck = Deck $ Cards [ Card suit rank | suit <- [Clubs .. ], rank <- [Ace .. ] ]