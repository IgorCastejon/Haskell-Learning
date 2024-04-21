{-# OPTIONS_GHC -Wno-typed-holes #-}

data Request
    = IsGet
    | IsNotGet
    deriving (Enum, Show)
data Body
    = FileWithHash
    | FileWithoutHash
    | DefaultBody
    | NullBody
    deriving (Enum, Show)
data Status
    = SetStatus
    | NotSetStatus
    deriving (Enum, Show)

data PossibleResponse 
    = Response Request Status Body
    deriving (Show)

mustCache :: Request -> Status -> Body -> Bool
mustCache IsNotGet _ _ = False
mustCache _ SetStatus _ = False
mustCache _ _ FileWithoutHash = False
mustCache _ _ NullBody = False
mustCache _ _ _ = True

allPossiblePairings :: [(Request, Status, Body)]
allPossiblePairings = [(x, y, z) | x <- [IsGet ..], y <- [SetStatus ..], z <- [FileWithHash ..]]

allPossiblePairingsPrettyString :: [String]
allPossiblePairingsPrettyString =
    map (\(x, y, z) -> prettyPrint (x, y, z, mustCache x y z) (maximum $ map (length . show) [IsGet ..])  (maximum $ map (length . show) [SetStatus ..]) (maximum $ map (length . show) [FileWithHash ..])) allPossiblePairings
    where
        prettyPrint :: (Request, Status, Body, Bool) -> Int -> Int -> Int -> String
        prettyPrint (x, y, z, w) lenMaxX lenMaxY lenMaxZ = 
            part x lenMaxX ++ " | " ++ part y lenMaxY ++ " | " ++ part z lenMaxZ ++ " | " ++ show w ++ "\n"
            where
                part :: (Show a) => a -> Int -> String
                part any maxLen = show any ++ replicate (maxLen - length (show any)) ' '


class (Show a, Show b) => (TruthTable a b) where
    value :: a -> b

data Test 
    = Nil
    | NotNil
    deriving (Show)

instance (TruthTable Test Bool) where
    value :: Test -> Bool
    value Nil = False
    value NotNil = True

instance TruthTable PossibleResponse Bool where
    value :: PossibleResponse -> Bool
    value (Response x y z) = mustCache x y z