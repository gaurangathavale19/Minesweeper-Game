import System.Random
import Text.Read
import Data.List
import Data.Typeable


type Cell = Either Bool Int
type Board = [[Cell]]
data State = Hidden | Revealed | Flagged deriving (Eq)
type Revealed = [[State]]


generateBoard :: Int -> Int -> Int -> IO Board
generateBoard rows cols mines = do
    gen <- getStdGen
    let mineLocations = take mines(nub(randomRs (0, rows * cols - 1) gen))
    let isMine r c =
            elem (r * cols + c) mineLocations &&
            not (r == 0 && c == 0 || r == 0 && c == cols - 1 || r == rows - 1 && c == 0 || r == rows - 1 && c == cols - 1)
    let countMines r c =
            length [() | dr <- [-1..1], dc <- [-1..1], isValid (r + dr) (c + dc), isMine (r + dr) (c + dc), dr /= 0 || dc /= 0]
          where
            isValid r' c' = r' >= 0 && r' < rows && c' >= 0 && c' < cols
    return [[if isMine r c then Left True else Right (countMines r c) | c <- [0..cols-1]] | r <- [0..rows-1]]


revealCell :: Board -> Revealed -> (Int, Int) -> Revealed
revealCell board revealed (r, c) 
    | r < 0 || r >= length board || c < 0 || c >= length (head board) = revealed
    | revealed !! r !! c == Revealed = revealed
    | otherwise = 
        let revealed' = map (\(ri, row) -> map (\(ci, rev) -> if (ri, ci) == (r, c) then Revealed else rev) (zip [0..] row)) (zip [0..] revealed)
        in case board !! r !! c of
            Right 0 -> foldl (\rev (dr, dc) -> revealCell board rev (r + dr, c + dc)) revealed' [(dr, dc) | dr <- [-1..1], dc <- [-1..1], dr /= 0 || dc /= 0]
            _ -> revealed'


printBoard :: Board -> Revealed -> IO ()
printBoard board revealed = do
    let indices = "    " ++ unwords (map show [0..length (head board)-1])
    putStrLn indices
    putStrLn $ replicate (4 * length (head board) + 3) '-'
    mapM_ printRow (zip [0..] $ zip board revealed)
    putStrLn $ replicate (4 * length (head board) + 3) '-'
  where
    printRow (i, (row, rev)) = putStrLn $ show i ++ " | " ++ unwords [if r == Revealed then case cell of Left _ -> "*"; Right n -> show n else if r == Flagged then "F" else "#" | (cell, r) <- zip row rev] ++ " |"


checkWin :: Board -> Revealed -> Bool
checkWin board revealed = all (== Revealed) [rev | (row, revRow) <- zip board revealed, (cell, rev) <- zip row revRow, cell /= Left True]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _        = False

-- Function to get a list of unrevealed cells on the board
unrevealedCells :: Board -> Revealed -> [[Int]]
unrevealedCells board revealed = [[r, c] | r <- [0..length board - 1], c <- [0..length (board !! 0) - 1], revealed !! r !! c == Hidden]

-- Function to get a list of unrevealed non-mine cells on the board
unrevealedNonMineCells :: Board -> Revealed -> [[Int]]
unrevealedNonMineCells board revealed = [[r, c] | r <- [0..length board - 1], c <- [0..length (board !! 0) - 1], revealed !! r !! c == Hidden, isRight (board !! r !! c)]

-- Function to get a list of total non-mine cells on the board
nonMineCells :: Board -> [[Int]]
nonMineCells board = [[r, c] | r <- [0..length board - 1], c <- [0..length (board !! 0) - 1], isRight (board !! r !! c)]

-- Function to randomly choose an unrevealed cell from the board
randomUnrevealedCell board revealed = do
    gen <- newStdGen
    let unrevealed = unrevealedCells board revealed
    let index = fst $ randomR (0, length unrevealed - 1) gen
    let randomCell = unrevealed !! index
    return randomCell

gameLoop :: Board -> Revealed -> IO ()
gameLoop board revealed = do
    printBoard board revealed
    if checkWin board revealed
        then 
            do
            putStrLn "Congratulations! You've won the game!"
            printBoard board (map (map (const Revealed)) revealed)
    else do
        random_hidden_cell <- randomUnrevealedCell board revealed
        let rc = [show a | a <- random_hidden_cell]
        putStrLn ("Random Cell Selected: " ++ (rc !! 0 ) ++ " " ++ (rc !! 1 ) )
        case rc of
            [r, c] -> case map readMaybe [r, c] :: [Maybe Int] of
                [Just r, Just c] -> case board !! r !! c of
                    Left _ -> do
                        putStrLn "Game Over! You revealed a mine."
                        let total_cells = ((length board)*(length (board !! 0)))
                        let total_non_mine_cells = length (nonMineCells board)
                        let unrevealed_cells = total_non_mine_cells - (length (unrevealedNonMineCells board revealed))
                        -- putStrLn ("unrevealed_cells: " ++ (show unrevealed_cells) ++ " total_non_mine_cells: " ++ (show total_non_mine_cells))
                        let result = fromIntegral (unrevealed_cells) * 100 / fromIntegral (total_non_mine_cells)
                        printBoard board (map (map (const Revealed)) revealed)
                        putStrLn ("Accuracy (revealed non-mine cells / total non-mine cells): " ++ (show result))
                    Right n -> if n == 0 then gameLoop board (revealCell board revealed (r, c)) else gameLoop board (revealCell board revealed (r, c))
                _ -> do
                    putStrLn "Invalid input. Please enter two numbers."
                    gameLoop board revealed
            _ -> do
                putStrLn "Invalid input. Please enter two numbers."
                gameLoop board revealed


main :: IO ()
main = do
    (rows, cols, density) <- getDifficulty
    let mines = max 1 (round ((fromIntegral (rows * cols) * (fromIntegral density / 100)) :: Float)) -- Ensure at least 1 mine
    board <- generateBoard rows cols mines
    gameLoop board (replicate rows (replicate cols Hidden))


getDifficulty :: IO (Int, Int, Int)
getDifficulty = do
    putStrLn "Please enter the difficulty (1 to 4):\n1. Easy\n2. Medium\n3. Hard\n4. Custom"
    choice <- getLine
    case choice of
        "1" -> return (6, 8, 21)
        "2" -> return (14, 18, 16)
        "3" -> return (20, 24, 21)
        "4" -> do
            putStrLn "Enter the number of rows:"
            rows <- readLn
            putStrLn "Enter the number of columns:"
            cols <- readLn
            putStrLn "Enter the mine density (as a percentage between 0 and 100):"
            density <- readLn
            return (rows, cols, density)
        _ -> do
            putStrLn "Invalid Input! Please enter a valid choice!!\n"
            getDifficulty