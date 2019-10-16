import Data.List (intercalate, delete)
import Data.Char (isSpace)
import qualified Data.Ratio as R
import Text.ParserCombinators.Parsec
import Data.Complex

newtype Poly = Poly [Rational]
    deriving Eq

s :: Poly
s = Poly [1, 0]

(%) :: Fractional a => Integer -> Integer -> a
x % y = fromRational ((R.%) x y)

instance Num Poly where
    (Poly a) + (Poly b) = Poly $ dropWhile (== 0) . reverse . take max_len $ 
        zipWith (+) (reverse a ++ repeat 0) (reverse b ++ repeat 0) where 
            max_len = max (length a) (length b)
    (Poly a) * (Poly b) = foldr1 (+) polys where
            polys = map Poly $ zipWith (\x y -> map (*y) x) dups b
            dups = scanr (\_ y -> y ++ [0]) a [1..length b - 1]

    negate (Poly xs) = Poly $ map (* (-1)) xs
    fromInteger i = Poly [fromInteger i]

instance Fractional Poly where
    fromRational f = Poly [fromRational f]

instance Show Poly where
    show (Poly a) = if length a == 1 then if (R.denominator (head a) == 1) then
        show (R.numerator (head a)) else filter (/= ' ') $ show $ head a else 
        intercalate " + " . words . concat $ terms where
        terms = zipWith helper a expa 
        expa = reverse [0..length a - 1]
        helper coff exp = if coff == 0 then " " 
            else " " ++ coff' ++ exp' ++ " " where
            coff'   | coff == 1 && exp /= 0 = ""
                    | coff == -1 && exp /= 0 = "-"
                    | otherwise = if (R.denominator coff) == 1 then 
                        show (R.numerator coff) else filter (/= ' ') $ show coff 
            exp'    | exp == 0 = ""
                    | exp == 1 = "s"
                    | otherwise = "s^" ++ show exp

pempty :: Poly -> Bool
pempty (Poly a) = null a

pdiv :: Poly -> Poly -> (Poly, Poly)
pdiv (Poly a) (Poly b)  | length a < length b = (0, Poly a)
                        | length a == length b = (q1, r1)
                        | otherwise = (q2, r2) where
                                q1 = Poly [head a / head b]
                                q2 = Poly $ (head a / head b) : zeros
                                zeros = replicate (length a - length b) 0
                                r1' = Poly a - q1 * (Poly b)
                                r2' = Poly a - q2 * (Poly b)
                                r1 = if pempty r1' then (Poly [0]) else r1'
                                r2 = if pempty r2' then (Poly [0]) else r2'

cauer1' :: Poly -> Poly -> String
cauer1' a b =
    let (q, r) = pdiv a b
    in if r == 0 then (show q) else 
        (show q) ++ " + 1/(" ++ (cauer1' b r) ++ ")"

{-strip :: String -> String-}
{-strip = reverse . dropWhile (isSpace) . reverse . dropWhile (isSpace)-}

cauer1 :: String -> String -> Either ParseError String
cauer1 a b = do
    pa <- parse getPoly "parsing numerator failure" a
    pb <- parse getPoly "parsing denominator failure" b
    return (cauer1' pa pb)

getTerm1 :: Parser Poly 
getTerm1 = do
    spaces
    a <- many (noneOf "+s")
    char 's'
    spaces
    char '^'
    b <- many (noneOf "+")
    if null a then return $ Poly $ 1 : (replicate (read b :: Int) 0) else
        if any (`elem` "%") a then return $ Poly $
        (read a :: Rational) : (replicate (read b :: Int) 0) else return $ Poly
        $ (toRational (read a :: Double)) : (replicate (read b :: Int) 0)

getTerm2 :: Parser Poly
getTerm2 = do
    spaces
    a <- many (noneOf "+s")
    b <- (char 's' >>= \_ -> return "1")
    spaces
    if null a then Poly <$> (return [1, 0]) else
        if any (`elem` "%") a then return $ Poly [read a :: Rational, 0] else
        return $ Poly [toRational (read a :: Double), 0]

getTerm3 :: Parser Poly
getTerm3 = do
    spaces
    a <- many (noneOf "+")
    if any (`elem` "%") a then return $ Poly [read a :: Rational] else
        return $ Poly [toRational (read a :: Double)]

getTerm :: Parser Poly
getTerm = try getTerm1 <|> try getTerm2 <|> getTerm3

getPoly :: Parser Poly
getPoly = chainl1 getTerm (op) where
    op = char '+' >> return (\x y -> x + y)

interpret :: String -> Poly
interpret = (\(Right x) -> x) . parse getPoly ""

eval' :: RealFloat a => Poly -> Complex a -> Complex a
eval' p val = foldl1 (\acc c -> acc * val + c) p' where
    p' = map fromRational . internal $ p
    internal (Poly x) = x

eval :: RealFloat a => String -> Complex a -> Complex a
eval s v = eval' p v where
    p = interpret s

extract :: Eq a => [a] -> [(a, [a])]
extract ls = zipWith helper ls (repeat ls) where
    helper a as = (a, delete a as)

is_root :: RealFloat a => Poly -> [Complex a] -> Bool
is_root p roots = null . filter mag $ map (eval' p) roots where
    mag c = magnitude c > 1e-10

order :: Poly -> Int
order (Poly x) = length x - 1

solve :: RealFloat a => Poly -> [Complex a]
solve p = let ini = map ((*(1 :+ 1)) . fromIntegral) [1..(order p)]
            in solve' p ini

solve' :: RealFloat a => Poly -> [Complex a] -> [Complex a] -- Durand–Kerner method
solve' p roots = if is_root p roots
    then roots else solve' p new_roots where
        new_roots = func roots
        func rs = map func' (extract rs)
        func' (a, as) =
            a - (eval' p a) / (product $ map (\v -> a - v) as)

p = interpret "s^3 + -3s^2 + 3s + -5"

hsolve :: Poly -> [String]
hsolve p = let result = solve p in
    map gen_str result where
        gen_str c = show realpart ++ " + " ++ show imagpart ++ "j" where
            realpart = (fromIntegral $ round $ realPart c * 10^n) / 10^n
            imagpart = (fromIntegral $ round $ imagPart c * 10^n) / 10^n
            n = 5

showRoot :: Poly -> IO ()
showRoot p = do
    putStrLn $ "Root of \"" ++ show p ++ "\":"
    mapM_ putStrLn $ zipWith (\n r -> show n ++ ". " ++ r) [1..] (hsolve p)
