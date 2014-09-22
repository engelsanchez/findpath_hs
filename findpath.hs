{-
 - Takes an ASCII map as input and finds a path between a starting and
 - ending position using the A* search algorithm.
 - In the input map spaces represent available cells,
 - the character 'o' is the starting position, the character 'X' is
 - the ending position and any other character is considered an
 - unavailable cell:
 -
 - ******************************
 - *********   *****     X   ****
 - **   o      *******     ******
 - ***                     ******
 - **......               ...****
 - ******************************
 -
 - The solution is printed by extending 'o' chars all the way to the 'X':
 -
 - ******************************
 - *********   *****   ooX   ****
 - **   ooooooo*******o    ******
 - ***         ooooooo     ******
 - **......               ...****
 - ******************************
 -
 -}

import Debug.Trace
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Heap as H

type Cell = (Int, Int)
type Path = [Cell]
type CellGrid = [[Char]]
data CellLink = CellLink Cell (Maybe CellLink) deriving (Show)
data CellDist = CellDist CellLink Double deriving (Show)
type Candidates = (H.MinHeap CellDist, S.Set CellLink) 

instance Ord CellDist where
   (CellDist _ w1) <= (CellDist _ w2) = w1 <= w2

instance Eq CellDist where
    (CellDist c1 _) == (CellDist c2 _) = c1 == c2
 
instance Eq CellLink where
    (CellLink c1 _) == (CellLink c2 _) = c1 == c2

instance Ord CellLink where
    CellLink c1 _ <= CellLink c2 _ = c1 <= c2

main :: IO ()
main = interact parseAndFindPath

parseAndFindPath :: String -> String
parseAndFindPath input =
    let
        grid = lines input
        Just (start, target) = findStartTarget grid
        echoOut = drawPath grid []
    in
        case findPath grid start target of
            Nothing -> "Input:\n" ++ echoOut ++ "\nNo path found\n"
            Just path ->
                "Input:\n" ++ echoOut ++ "\nSolution:\n" ++ resultOut
                where
                    resultOut =  drawPath grid $ drop 1 path

findStartTarget :: [[Char]] -> Maybe (Cell, Cell)
findStartTarget grid =
    case (start, target) of
        ([], _) -> Nothing
        (_, []) -> Nothing
        ([s], [e]) -> Just (s, e)
    where
        start = take 1 $ findCharInGrid grid 'o'
        target = take 1 $ findCharInGrid grid 'X'

findCharInGrid :: [[Char]] -> Char -> [Cell]
findCharInGrid grid char = concat $ map addRow $ zip [0..] $ map (findChar char) grid

findChar :: Char -> [Char] -> [Int]
findChar char chars = 
    map (fst) $ filter ((== char) . snd) $ zip [0..] chars

addRow :: (a, [b]) -> [(a,b)]
addRow (n, l) = zip (repeat n) $ l

findPath :: CellGrid -> Cell -> Cell -> Maybe Path
findPath cells start target
    | start == target = Just $ toPath $ CellLink start Nothing
findPath cells start target =
    let
        startLink = CellLink start Nothing
        candidates = addCandidate (newCandidates) (addDist2 target startLink)
        seen = S.fromList [start]
    in
        search cells target candidates seen

toLink :: CellLink -> Cell -> CellLink
toLink prev cell = CellLink cell (Just prev)

toPath :: CellLink -> Path
toPath (CellLink cell Nothing) = [cell]
toPath (CellLink cell (Just cellLink)) = cell : toPath cellLink

newCandidates :: Candidates
newCandidates = (H.empty, S.empty)

addCandidate :: Candidates -> CellDist -> Candidates
addCandidate (heap, set) cd
    | S.member (cellLink cd) set = (heap, set)
    | otherwise = (newHeap, newSet)
        where
            newHeap = H.insert cd heap
            newSet  = S.insert (cellLink cd) set

search :: CellGrid -> Cell -> Candidates -> S.Set Cell -> Maybe Path
search cells target candidates seen
    | Main.null candidates = Nothing
    | otherwise =
        let (nextToVisit, cand1) = findBest candidates
            nextCell = cell nextToVisit
            neighbors = findNeighbors cells nextCell
            newNeighbors = notIn seen neighbors
            newNeighborSet = S.fromList $ newNeighbors
            newSeen = S.union seen newNeighborSet
            neighborWeights = toCellDists newNeighbors nextToVisit target
            newCands = foldl (addCandidate) cand1 neighborWeights
            reachedTarget = S.member target newNeighborSet
        in
            if reachedTarget
            then
                Just (toPath $ toLink nextToVisit target)
            else
                search cells target newCands newSeen

findBest :: Candidates -> (CellLink, Candidates)
findBest (heap, set) = (best, (newHeap, newSet))
    where
        Just ((CellDist best _), newHeap) = H.view heap
        newSet = S.delete best set

cell :: CellLink -> Cell
cell (CellLink cl _) = cl

cellLink :: CellDist -> CellLink
cellLink (CellDist cl _) = cl

findNeighbors :: CellGrid -> Cell -> [Cell]
findNeighbors cells (row, col)
    | row < 0 = []
    | row >= length cells = []
    | otherwise = concat $ map extract $ rows
        where 
            rows = [row-1..row+1]
            extract n = zip (repeat n) $ availableCells (cells !! n) (col-1) 3

availableCells :: [Char] -> Int -> Int -> [Int]
availableCells cellList col num
    | col < 0 = availableCells cellList 0 $ max 0 num+col
availableCells _ _ 0 = []
availableCells cellList col num =
    let subList = zip [col..col+num-1] $ drop col cellList
        isAvailable (_, char) = (char == ' ') || (char == 'X')
    in  
        take num $ map fst $ filter (isAvailable) subList

drawPath :: CellGrid -> Path -> String
drawPath grid path =
    output ++ "\n\n"
    where
        pathSet = S.fromList path
        numbered = zip [0..] grid 
        output = L.intercalate "\n" $ map (addPath pathSet) numbered

addPath :: S.Set Cell -> (Int, [Char]) -> [Char]
addPath set (row, cols) = map (outChar set) $ zip3 (repeat row) [0..] cols
        
outChar :: S.Set Cell -> (Int, Int, Char) -> Char
outChar overlay (row, col, char) =
    if S.member (row, col) overlay then 'o' else char
    
dist2 :: Cell -> Cell -> Double
dist2 (r1, c1) (r2, c2) = (fromIntegral (r1-r2))^2 + (fromIntegral (c1-c2))^2
 
null :: Candidates -> Bool
null (_, set) = S.null set

toCellDists :: [Cell] -> CellLink -> Cell -> [CellDist]
toCellDists cells prevCell target =
    map (addDist2 target . toLink prevCell) cells

addDist2 :: Cell -> CellLink -> CellDist 
addDist2 target cl = CellDist cl (dist2 target (cell cl))

notIn :: S.Set Cell -> [Cell] -> [Cell]
notIn visited cells = filter (not . (`S.member` visited)) cells


