module Main(main) where

import Graphics.Gloss
-- import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game

initLoc :: (Float, Float)
initLoc = (0,0)

data GameState = Game
    {
        pLoc :: (Float, Float)
        , pSpeed :: (Float, Float)

    } deriving Show

initialState :: GameState
initialState = Game
    {
        pLoc = initLoc
        , pSpeed = (0,0)
    }

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = white
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update



-- | Convert a game state into a picture.
render :: GameState  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, testquare]
  where
    --  The pong ball.
    ball = uncurry translate (pLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red
    testquare = translate 10 20 $ color (light blue) $ rectangleSolid 20 30



-- | Respond to key events.
handleKeys :: Event -> GameState -> GameState

-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 'x') _ _ _) game =
  game { pLoc = initLoc }
handleKeys (EventKey (Char 'w') _ _ _) game =
  game { pSpeed = ((fst (pSpeed game)), (snd  (pSpeed game)) + 2) }

handleKeys (EventKey (Char 'a') _ _ _) game =
  game { pSpeed = ((fst  (pSpeed game)) - 2, (snd (pSpeed game))) }

handleKeys (EventKey (Char 's') _ _ _) game =
  game { pSpeed = ((fst (pSpeed game)), (snd  (pSpeed game)) - 2) }

handleKeys (EventKey (Char 'd') _ _ _) game =
  game { pSpeed = ((fst (pSpeed game)) + 2, (snd  (pSpeed game)) ) }

-- Do nothing for all other events.
handleKeys _ game = game




-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> GameState -> GameState
update  seconds =  movePlayer seconds


movePlayer :: Float -> GameState -> GameState
movePlayer seconds game = game {pLoc = (x', y') }
    where
        (x, y) = pLoc game
        (vx, vy) = pSpeed game   
        x' = x + vx * seconds * 10
        y' = y + vy * seconds * 10