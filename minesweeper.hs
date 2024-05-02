{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text (Text)
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class (liftIO)
import System.Random
-- import Text.Read
import Data.IORef
import Data.List
-- import Data.Either (fromRight)
-- import Web.Scotty.Trans
import Data.Aeson
import GHC.Generics
import Data.Aeson (object, (.=), encode, ToJSON)
import Data.Text.Lazy.Encoding (decodeUtf8)




type Cell = Either Bool Int
type Board = [[Cell]]
data State = Hidden | Revealed | Flagged deriving (Eq)

instance ToJSON State where
    toJSON Hidden = String "Hidden"
    toJSON Revealed = String "Revealed"
    toJSON Flagged = String "Flagged"

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

toggleFlag :: State -> State
toggleFlag Hidden = Flagged
toggleFlag Flagged = Hidden
toggleFlag Revealed = Revealed

update2DList :: [[State]] -> Int -> Int -> [[State]]
update2DList xs row col =
    [ if r /= row then x else [ if c /= col then y else toggleFlag y | (y,c) <- zip x [0..] ] | (x,r) <- zip xs [0..] ]

main = do
  
  boardRef <- newIORef [] -- Create an IORef for the board
  revealedRef <- newIORef [] -- Create an IORef for the revealed cells

  scotty 3000 $ do

    post "/submit" $ do
      option <- formParam "option" :: ActionM Text
      -- let option = fst $ fromRight (0, pack "") $ decimal optionText
      (rows, cols, density) <- case option of
        "1" -> return (6, 8, 21)
        "2" -> return (14, 18, 16)
        "3" -> return (20, 24, 21)
        "4" -> do
          customRows <- formParam "rows" :: ActionM Int
          customCols <- formParam "cols" :: ActionM Int
          customDensity <- formParam "density" :: ActionM Int
          return (customRows, customCols, customDensity)
      redirect $ mconcat ["/other-page?rows=", pack (show rows), "&cols=", pack (show cols), "&density=", pack (show density)]
      -- redirect $ mconcat ["/other-page?option=", option]

    get "/other-page" $ do
      -- input1 <- param "rows" 
      -- liftIO $ putStrLn input1
      -- text $ mconcat ["You submitted: ", input1]
      rows <- param "rows" :: ActionM Int
      cols <- param "cols" :: ActionM Int
      density <- param "density" :: ActionM Int
      let mines = max 1 (round ((fromIntegral (rows * cols) * (fromIntegral density / 100)) :: Float)) -- Ensure at least 1 mine
      board <- liftIO $ generateBoard rows cols mines
      let revealed = replicate rows (replicate cols Hidden)
      liftIO $ writeIORef boardRef board
      liftIO $ writeIORef revealedRef revealed
      -- gameLoop board (replicate rows (replicate cols Hidden))
      file "temp.html"
      -- redirect $ mconcat ["/play?rows=", pack (show rows), "&cols=", pack (show cols), "&density=", pack (show density)]
    
    post "/play" $ do
      row <- param "row" :: ActionM Int
      col <- param "col" :: ActionM Int
      -- density <- param "density" :: ActionM Int
      -- board <- param "board" :: ActionM Board
      -- revealed <- param "revealed" :: ActionM Revealed
      board <- liftIO $ readIORef boardRef
      revealed <- liftIO $ readIORef revealedRef
      liftIO $ writeIORef revealedRef (revealCell board revealed (row, col))
      liftIO $ putStrLn (show row)
      revealed <- liftIO $ readIORef revealedRef
      liftIO $ printBoard board revealed
      -- redirect $ mconcat ["/other-page?rows=", pack (show row), "&cols=", pack (show col), "&density=", pack (show density)]
      -- file "temp.html"
      json $ object ["board" .= board, "revealed" .= revealed]
      

    post "/flag" $ do
      row <- param "row" :: ActionM Int
      col <- param "col" :: ActionM Int
      -- status <- param "status" :: ActionM String
      -- density <- param "density" :: ActionM Int
      -- board <- param "board" :: ActionM Board
      -- revealed <- param "revealed" :: ActionM Revealed
      board <- liftIO $ readIORef boardRef
      revealed <- liftIO $ readIORef revealedRef
      liftIO $ writeIORef revealedRef (update2DList revealed row col)
      -- revealed[row][col] = Flagged
      liftIO $ putStrLn (show row)
      revealed <- liftIO $ readIORef revealedRef
      liftIO $ printBoard board revealed
      -- redirect $ mconcat ["/other-page?rows=", pack (show row), "&cols=", pack (show col), "&density=", pack (show density)]
      -- file "temp.html"
      json $ object ["board" .= board, "revealed" .= revealed]



