module Engine where

import qualified Data.Set as S
import qualified Data.Map.Lazy as M
import qualified Data.List as L
import Data.Maybe

type Tile = Char
type Coords = (Int, Int)
type Board = M.Map Coords Tile
data State = State Board [Tile]

completed :: State -> Bool
completed (State board []) = isValidBoard board
completed _ = False

-- a board is valid if all the tiles participate in two valid words
isValidBoard :: Board -> Bool
isValidBoard board = let coords = map fst (M.toAscList board)
                      in all (isValidTile board) coords

isValidTile :: Board -> Coords -> Bool
isValidTile board c = (isValidWord $ horizontalWord board c) &&
                      (isValidWord $ verticalWord board c)

-- finding words
horizontalWord :: Board -> Coords -> [Tile]
horizontalWord board c = let start = startOfHorizontalWord board c
                             coords = L.unfoldr (moveRight board) start
                          in map (fromJust . (flip M.lookup $ board)) coords

startOfHorizontalWord :: Board -> Coords -> Coords
startOfHorizontalWord board c = head $ L.unfoldr (moveLeft board) c

verticalWord :: Board -> Coords -> [Tile]
verticalWord board c = let start = startOfVerticalWord board c
                           coords = L.unfoldr (moveDown board) start
                        in map (fromJust . (flip M.lookup $ board)) coords

startOfVerticalWord :: Board -> Coords -> Coords
startOfVerticalWord board c = head $ L.unfoldr (moveUp board) c

-- Moves on the board
moveDirection :: (Coords -> Coords) -> Board -> Coords -> Maybe (Coords, Coords)
moveDirection move board (x, y) = if (move (x, y)) `M.member` board
                                     then Just ((x, y), move (x, y))
                                     else Nothing

moveLeft :: Board -> Coords -> Maybe (Coords, Coords)
moveLeft = moveDirection (\(x, y) -> (x-1, y))

moveRight :: Board -> Coords -> Maybe (Coords, Coords)
moveRight = moveDirection (\(x, y) -> (x+1, y))

moveUp :: Board -> Coords -> Maybe (Coords, Coords)
moveUp = moveDirection (\(x, y) -> (x, y-1))

moveDown :: Board -> Coords -> Maybe (Coords, Coords)
moveDown = moveDirection (\(x, y) -> (x, y+1))

-- a word is valid if it is of length one, or if it is in the dictionary
isValidWord :: [Tile] -> Bool
isValidWord (t:[]) = True
isValidWord word = S.member word dictionary

dictionary :: S.Set String
dictionary = S.fromList ["cat", "dog", "add"]
