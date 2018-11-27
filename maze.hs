import qualified System.Environment

type Cell = (Int,Int)

main :: IO ()
main = do
  [path] <- System.Environment.getArgs                                                          -- get the path to the maze textfile
  maze <- readFile path                                                                         -- read the text file
  let mazeLines = lines maze                                                                    -- String -> [String], each sublist is one line of the maze
  let width     = length $ head mazeLines                                                       -- width of maze
  let height    = length mazeLines                                                              -- height of maze
  let start     = locateStart mazeLines                                                         -- find the cell at which the path will start
  let goal      = locateGoal mazeLines                                                          -- find the cell the path has to reach
  let path      = snip $ fromJust $ findPath start goal mazeLines width height                  -- calculate shortest path
  let boolMap   = [[elem (i,j) path | i <- [0..width-1] ] | j <- [0..height-1]]                 -- each cell that belongs to the path will be marked True, see bottom of document
  let result    = drawPath boolMap mazeLines width height                                       -- format result
  putStrLn $ unlines result                                                                     -- print result on screen

getElem :: Cell -> [String] -> Char
getElem cell maze = (maze !! (snd cell)) !! (fst cell)

getBool :: Cell -> [[Bool]] -> Bool
getBool cell bools = (bools !! (snd cell)) !! (fst cell)

locateStart :: [String] -> Cell
locateStart maze = head [(i, j) | i <- [0..((length (maze !! 0)) - 1)],
                                  j <- [0..((length maze) - 1)],
                                  getElem (i, j) maze == '*']

locateGoal :: [String] -> Cell
locateGoal maze = head [(i, j) | i <- [0..((length (maze !! 0)) - 1)],
                                 j <- [0..((length maze) - 1)],
                                 getElem (i, j) maze == '@']

snip :: [Cell] -> [Cell] -- remove first and last element of list
snip p = reverse $ tail $ reverse $ tail p

fromJust :: Maybe a -> a
fromJust Nothing = error "fromJust: Nothing -> No path found"
fromJust (Just x) = x

findPath :: Cell -> Cell -> [String] -> Int -> Int -> Maybe [Cell]
findPath start goal maze w h = pathAux start goal maze w h []

pathAux :: Cell -> Cell -> [String] -> Int -> Int -> [Cell] -> Maybe [Cell]
pathAux curr goal maze width height visited
    | curr == goal                                   = Just [curr]
    | isWall                                         = Nothing
    | notVisited west  && nextCell west  /= Nothing  = Just (curr : (fromJust (nextCell west)))
    | notVisited north && nextCell north /= Nothing  = Just (curr : (fromJust (nextCell north)))
    | notVisited east  && nextCell east  /= Nothing  = Just (curr : (fromJust (nextCell east)))
    | notVisited south && nextCell south /= Nothing  = Just (curr : (fromJust (nextCell south)))
    | otherwise                                      = Nothing
    where currX       = fst curr
          currY       = snd curr
          west        = (currX - 1, currY)
          north       = (currX, currY - 1)
          east        = (currX + 1, currY)
          south       = (currX, currY + 1)
          newVisited  = curr:visited
          isWall      = (getElem (currX,currY) maze) == 'X'
          notVisited ::  Cell -> Bool
          notVisited cell = not (elem cell visited)
          nextCell :: Cell -> Maybe [Cell]
          nextCell newCurr  = pathAux newCurr goal maze width height newVisited


drawPath :: [[Bool]] -> [String] -> Int -> Int -> [String]
drawPath boolMap maze w h = [[(getMazeChar boolMap maze i j) | i <- [0..w-1] ] | j <- [0..h-1]]

getMazeChar :: [[Bool]] -> [String] -> Int -> Int -> Char
getMazeChar boolMap maze x y = if (getBool (x,y) boolMap)  -- True  => cell is part of path, draw '.'
                              then '.'
                              else (getElem (x,y) maze)    -- False => cell not part of path, draw normal maze





-- boolMap:
-- [False,False,False,False,False,False,False,False,False,False,False,False,False,False],
-- [False,True, False,False,False,False,False,False,False,False,False,False,False,False],
-- [False,True, True, True, True, True, True, True, True, True, True, False,False,False],
-- [False,False,False,False,False,False,False,False,False,False,True, False,False,False],
-- [False,False,False,False,False,False,False, True, True, True,True, False,False,False],
-- [False,False,False,False,False,False,False,False,False,False,False,False,False,False],
-- [False,False,False,False,False,False,False,False,False,False,False,False,False,False]
