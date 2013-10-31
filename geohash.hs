import Data.Bits
import Data.Array
import Data.Char
import Data.List
import Data.Maybe

-- sample
-- geohash u33dc0f42e445
-- lat lon 52.5191710 13.4060912

base32 = "0123456789bcdefghjkmnpqrstuvwxyz"
base32Idx = zip base32 [0..]

-- 0 right, 1 left, 2 top, 3 down
neighbors = ["bc01fg45238967deuvhjyznpkmstqrwx","238967debc01fg45kmstqrwxuvhjyznp","p0r21436x8zb9dcf5h7kjnmqesgutwvy","14365h7k9dcfesgujnmqp0r2twvyx8zb"]
borders = ["bcfguvyz", "0145hjnp", "prxz", "028b"]

-- U33E0 E33E1 U33E4
-- U33DB U33DC U33DF
-- U33D8 U33D9 U33DD

-- | Return geohash for neighbor in direction
--
-- neighbor "u33dc" 2
neighbor :: String -- ^ geohash
            -> Int  -- ^ direction  @(0 - right, 1 - left, 2 - top, 3 - down)@
            -> String  -- ^ neighbor geohash
neighbor geohash dir = base ++ res:[]
  where
    index = ((2 * ((length geohash) `mod` 2)) + dir) `mod` 4
    base = if isJust (findIndex (== (last geohash)) (borders !! index)) then neighbor (init geohash) dir else init geohash
    res = base32 !! fromJust (findIndex (== (last geohash)) (neighbors !! index))

-- | Decode a geohash back into a @(lat,lon)@ pair.
--
-- decode "u33d"
decode :: (Fractional a)
          => String        -- ^ geohash
          -> Maybe (a,a)   -- ^ @(lat,lon)
decode x = case stringAsLatLonBits x of
  Just bits -> Just (mid $ posRange (-90,90) $ fst bits, mid $ posRange (-180,180) $ snd bits)
  Nothing -> Nothing

posRange :: (Fractional a) => (a,a) -> [Bool] -> (a,a)
posRange (min,max) [] = (min,max)
posRange (min,max) (x:xs)
  | x = posRange (mid,max) xs
  | otherwise = posRange (min,mid) xs
  where
    mid = min + ((max - min) / 2)

stringAsLatLonBits :: String -> Maybe ([Bool],[Bool])
stringAsLatLonBits x
  | all (isJust) bits = Just (splitLatLonBits $ concat $ catMaybes bits)
  | otherwise = Nothing
  where
    bits = map charAsBits x

splitLatLonBits :: [Bool] -> ([Bool],[Bool])
splitLatLonBits xs = (odds xs, evens xs)
  where
    evens []  = []
    evens [x] = [x]
    evens (x:_:xs) = x : evens xs
    odds []  = []
    odds [x] = []
    odds (_:x:xs) = x : odds xs

charAsBits :: Char -> Maybe [Bool]
charAsBits x = case find ((== x) . fst) base32Idx of
  Just (a,b) -> Just (intAsBits b)
  Nothing -> Nothing

intAsBits :: Int -> [Bool]
intAsBits x = padding ++ bits
  where
    bits = reverse $ map (\x' -> if x' == 1 then True else False) $ unfoldr (\x' -> if x' == 0 then Nothing else Just (x' `mod` 2, x' `div` 2)) x
    padding = take (5 - (length bits)) (repeat False)



-- | Encode a geohash from the given @(lat,lon) pair with the given
--  precision.
--
-- encode (52.34, 13.4) 8
encode :: (Fractional a, Ord a)    
          => (a,a)         -- ^ @(lat,lon)@
          -> Int           -- ^ precision
          -> Maybe String  -- ^ geohash
encode (lat,lon) p = case latLonAsBits (lat,lon) of
  Just bits -> Just (map (bitsAsChar) (take p $ splitEvery 5 bits))
  Nothing -> Nothing

latLonAsBits :: (Fractional a, Ord a) => (a,a) -> Maybe [Bool]
latLonAsBits (a,b) 
  | (isJust latBits) && (isJust lonBits) = Just (concat $ transpose [(fromJust lonBits),(fromJust latBits)])
  | otherwise = Nothing
  where
    latBits = asBits (-90,90) a
    lonBits = asBits (-180,180) b
    asBits (min,max) n = if (min < n) && (n < max) then Just (numberAsBits (min,max) n) else Nothing

numberAsBits :: (Fractional a, Ord a) => (a,a) -> a -> [Bool]                                                                                             
numberAsBits (min,max) x
  | x > mid = True:numberAsBits (mid,max) x
  | otherwise = False:numberAsBits (min,mid) x
  where
    mid = min + ((max - min) / 2)

bitsAsChar :: [Bool] -> Char
bitsAsChar xs = fst $ base32Idx !! (bitsAsInt xs)

bitsAsInt :: [Bool] -> Int
bitsAsInt x = sum $ map (2^) $ findIndices (== True) $ reverse x

    
-- |    
-- util functions
    
mid :: (Fractional a) => (a, a) -> a
mid (x,y) = (x + y)/2

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not.null) . map (take n) . iterate (drop n)