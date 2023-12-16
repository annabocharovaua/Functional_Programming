module Main where
import Control.Monad (guard)
import Data.List (minimumBy, sortBy)
import Data.Function (on)

type Position = (Int, Int)
type Board = [[Int]]

boardSize :: Int
boardSize = 8

-- Finding possible moves for the knight
possibleMoves :: [Position] -> Position -> [Position]
possibleMoves visited (x, y) = filter (`notElem` visited) (filter onBoard $ map move moves)
  where
    onBoard (a, b) = a >= 1 && a <= boardSize && b >= 1 && b <= boardSize
    move (dx, dy) = (x + dx, y + dy)
    moves = [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]
    boardSize = 8

-- Moving the knight on the chessboard according to the Warnsdorff algorithm
-- return type [Position] represents Maybe Position. [] if Nothing, [move] if Just. Because Maybe didn't work
moveKnight :: Position -> [Position] -> [Position]
moveKnight (x, y) visited = do
    let moves = possibleMoves visited (x, y)
    case moves of
      [] -> []
      nonEmptyMoves -> [minimumBy (compare `on` length . possibleMoves visited) moves]

knightsTour :: Int -> Position -> [Position]
knightsTour numberOfCells startPos = go numberOfCells [startPos] startPos
  where
    go 1 path _ = reverse path
    go numberOfCellsRemainingPlusOne visited currentPos =
      let nextPos = moveKnight currentPos visited
      in if length nextPos == 0 then []
         else go (numberOfCellsRemainingPlusOne - 1) (head nextPos : visited) (head nextPos)

main :: IO ()
main = do
  let start = (1, 1)
      numberOfCells = boardSize * boardSize
      tour = knightsTour numberOfCells start
  if length tour == 0 then putStrLn "No solution found."
  else putStrLn $ (show (length tour)) ++ " " ++ (show tour)