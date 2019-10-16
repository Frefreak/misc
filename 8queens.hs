import Data.List
import Data.Char

draw_helper :: Char -> IO ()
draw_helper ch = do
    let s = (map (\nch -> if ch == nch then 'Q' else '+') "01234567")
        in sequence_ (map (\c -> putStr ([c] ++ " ")) s)
    putStrLn ""

draw_board :: Board -> IO ()
draw_board [] = putStrLn ""
draw_board (x:xs) = do
    draw_helper x
    draw_board xs

type Board = String

gen_board :: [Board]
gen_board = permutations "01234567"

check_board :: Board -> Bool
check_board [] = True
check_board board@(b:bs) = check_helper board && check_board bs

check_helper :: String -> Bool
check_helper s@(x:_) =
    let l = length s
        in all (\n -> abs (ord x - ord (s!!n)) /= n) [1..l-1]

valid_board :: [Board]
valid_board = filter check_board gen_board

prettier_board :: (Int, Board) -> IO ()
prettier_board (n, b) = do
    putStrLn $ "     (" ++ show n ++ ")"
    draw_board b

queens :: IO ()
queens = sequence_ $ map prettier_board
    (zip [1..(length valid_board)] valid_board)

main :: IO ()
main = queens
