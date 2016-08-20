{-# LANGUAGE RecordWildCards #-}

module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.IO.Simulate
import           System.Random                        (randomIO)

cellRadius :: Float
cellRadius = 25.0

lineThickness :: Float
lineThickness = 1.5

cols :: Int
rows :: Int
(cols, rows) = (28, 18)

data Pos = Pos Float Float deriving (Show, Eq)

data Wall
  = Wall
  | Open
  deriving (Show, Eq)

data Cell
  = Cell
    { visited :: Bool
    , walls   :: [Wall]
    } deriving (Show, Eq)

data Model
  = Model
    { grid    :: [[Cell]]
    , current :: (Int, Int)
    , stack   :: [(Int, Int)]
    } deriving (Show, Eq)

main :: IO ()
main =
  simulateIO
    (FullScreen (1366, 768)) -- (InWindow "HexaMaze" (600, 600) (200, 50))
    (makeColor 0.322 0.318 0.455 1.0)
    60
    initialModel
    view
    update

segment :: (Pos, Pos) -> Picture
segment (Pos x1 y1, Pos x2 y2) = Pictures $
  [ polygon
    [ (x1 - dx, y1 - dy), (x1 + dx, y1 + dy)
    , (x2 + dx, y2 + dy), (x2 - dx, y2 - dy)
    ]
  , translate x1 y1 $ circleSolid lineThickness
  , translate x2 y2 $ circleSolid lineThickness
  ]
  where
    vx = x2 - x1
    vy = y2 - y1
    q = sqrt $ vx^2 + vy^2
    dx = - lineThickness * vy / q
    dy =   lineThickness * vx / q

getCellPos :: (Int, Int) -> Pos
getCellPos (i, j)
  | even j
  = Pos x y

  | otherwise
  = Pos (x - cellRadius * sqrt 3 / 2) y

  where
    x = fromIntegral i * cellRadius * sqrt 3
    y = fromIntegral j * 1.5 * cellRadius

getCellPicture :: Pos -> Picture
getCellPicture (Pos x y) = polygon $
  [ (x + cellRadius * cos t, y + cellRadius * sin t)
  | t <- map (\i -> pi * (1.5 - fromIntegral i) / 3) [0..5]
  ]

getWallSeg :: Pos -> Int -> (Pos, Pos)
getWallSeg (Pos x y) i = (Pos (x + dx1) (y + dy1), Pos (x + dx2) (y + dy2))
  where
    p1 = pi * (1.5 - fromIntegral (i `mod` 6)) / 3
    dx1 = cellRadius * cos p1
    dy1 = cellRadius * sin p1

    p2 = pi * (0.5 - fromIntegral (i `mod` 6)) / 3
    dx2 = cellRadius * cos p2
    dy2 = cellRadius * sin p2

modifyCell :: (Int, Int) -> (Cell -> Cell) -> [[Cell]] -> [[Cell]]
modifyCell (i, j) f grid = take j grid ++ [ newRow ] ++ drop (j+1) grid
  where
    row = grid !! j
    newRow = take i row ++ [ f $ row !! i ] ++ drop (i+1) row

visitCell :: Cell -> Cell
visitCell c = c { visited = True }

initialModel :: Model
initialModel =
  Model
    { grid    = modifyCell (0, 0) visitCell grid
    , current = (0, 0)
    , stack   = [(0, 0)]
    }
  where
    grid = replicate rows . replicate cols . Cell False $ replicate 6 Wall

gridLoopPicture :: ((Int, Int) -> Picture) -> Picture
gridLoopPicture f = Pictures
  [ Pictures $
    [ f (i, j)
    | i <- [0..cols-1]
    ]
  | j <- [0..rows-1]
  ]

view :: Model -> IO Picture
view Model{..} = return $
  translate
    (- sqrt 3 * cellRadius * fromIntegral cols / 2 + cellRadius)
    (- 1.5 * cellRadius * fromIntegral rows / 2 + cellRadius)
    $ Pictures
    [ cellsPic, wallsPic ]

  where
    cellsPic = gridLoopPicture $ \(i, j) ->
      let
        p           = getCellPos (i, j)
        cell        = grid !! j !! i
        cellPicture = getCellPicture p
      in
        color
          (if (i, j) == current
            then makeColor 0.737 0.906 0.518 1.0
            else if visited cell
              then makeColor 0.365 0.827 0.62 1.0
              else makeColor 0.204 0.541 0.655 1.0)
          cellPicture

    wallsPic = gridLoopPicture $ \(i, j) ->
      let
        p    = getCellPos (i, j)
        cell = grid !! j !! i
      in
        Pictures $
          [ if walls cell !! w == Wall
            then color
              (makeColor 0.318 0.231 0.337 1.0)
              (segment $ getWallSeg p w)
            else Blank
          | w <- [0..5]
          ]

getNeighbours :: (Int, Int) -> [Maybe (Int, Int)]
getNeighbours (i, j)
  | even j
  = map inBounds [(i+1, j+1), (i+1, j), (i+1, j-1), (i, j-1), (i-1, j), (i, j+1)]

  | otherwise
  = map inBounds [(i, j+1), (i+1, j), (i, j-1), (i-1, j-1), (i-1, j), (i-1, j+1)]

  where
    inBounds :: (Int, Int) -> Maybe (Int, Int)
    inBounds (i, j) = if 0 <= i && i < cols && 0 <= j && j < rows
      then Just (i, j)
      else Nothing

pickNext :: Model -> IO (Maybe (Int, (Int, Int)))
pickNext Model{..} =
  case indices of
    [] -> return Nothing
    ls -> fmap (\x -> let i = indices !! (x `mod` length ls)
                      in  sequence (i, neighbours !! i)
            ) randomIO
  where
    neighbours :: [Maybe (Int, Int)]
    neighbours = getNeighbours current

    indices :: [Int]
    indices = map fst . filter (select . snd) $ zip [0..] neighbours

    select :: Maybe (Int, Int) -> Bool
    select Nothing       = False
    select (Just (i, j)) = not . visited $ grid !! j !! i

removeWall :: Int -> Cell -> Cell
removeWall w Cell{..} = Cell visited $ take w walls ++ [ Open ] ++ drop (w + 1) walls

update :: ViewPort -> Float -> Model -> IO Model
update _ _ model@Model{..} = do
  next <- pickNext model

  case next of
    Nothing     ->
      return $ case stack of
        [] -> initialModel -- set to "model" to keep the generated maze
        _  -> model { current = head stack
                    , stack   = tail stack
                    }
    Just (w, p) ->
      return $ model { current = p
                     , grid    = modifyCell p visitCell
                               . modifyCell current (removeWall w)
                               . modifyCell p       (removeWall $ (w + 3) `mod` 6)
                               $ grid
                     , stack   = p : stack
                     }
