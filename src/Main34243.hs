module Main(main) where

import Graphics.Gloss
-- import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game

type Position = (Float, Float)
type SpeedVec = (Float, Float)


initLoc :: (Float, Float)
initLoc = (0,0)

data GameState = Game
    {
    --   pLoc :: (Float, Float)
    -- , pSpeed :: (Float, Float)
        player1 :: Player
        -- player2 :: Player
        -- gameScore :: Float
        -- bullets1 :: [Bullet]
        -- bullets2 :: [Bullet]

    } deriving Show


data Player = Player
  {
    pLoc :: (Float, Float)
    , pSpeedx :: Float
    , pSpeedy :: Float
    -- , isOnTheGeound :: Bool

  } deriving Show
data Bullet = Bullet
  {


  }
initialPlayer :: Player
initialPlayer = Player
  {
        pLoc = initLoc
        , pSpeedx = 0
        , pSpeedy = 0

  }


makePlayer :: Position -> SpeedVec ->  Player
makePlayer (x,y) (x', y') = Player {
  pLoc = (x,y)
  , pSpeedx = x'
  , pSpeedy = y'
}

initialState :: GameState
initialState = Game
    {
         player1 = initialPlayer
    }
-- initialState :: GameState
-- initialState = Game
--     {
--           pLoc = initLoc
--         , pSpeed = (0,0)
--     }

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
    ball = uncurry translate (pLoc (player1 game)) $ color ballColor $ circleSolid 10
    ballColor = dark red
    testquare = translate 10 20 $ color (light blue) $ rectangleSolid 20 30




-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> GameState -> GameState
update  seconds =  movePlayer seconds


movePlayer :: Float -> GameState -> GameState
movePlayer seconds game = game  { player1  = move (player1 game) }
    where
        move player = player {
        pLoc = (x', y')
      }
        (x, y) = pLoc (player1  game)
        -- (vx, vy) = pSpeed (player1  game)
        vx = pSpeedx (player1 game)
        vy = pSpeedy (player1 game)
        x' = x + vx * seconds * 10
        y' = y + vy * seconds * 10



speed :: Float
speed = 20

-- | Respond to key events.
handleKeys :: Event -> GameState -> GameState
handlekeys (EventKey (Char 'a') _ _ _ ) game = movePlayerLeft game
-- handlekeys (EventKey (Char 'a') _ _ _ ) game = stopPlayer game
handlekeys (EventKey (Char 'd') _ _ _ ) game = movePlayerRight game
-- handlekeys (EventKey (Char 'd') Up _ _ ) game = stopPlayer game




-- -- For an 's' keypress, reset the ball to the center.
-- handleKeys (EventKey (Char 'x') Down _ _) game =
--   game { pLoc = initLoc }
-- handleKeys (EventKey (Char 'w') Up _ _) game =
--   game { pSpeed = ((fst (pSpeed game)), (snd  (pSpeed game)) - speed) }
-- handleKeys (EventKey (Char 'w') Down _ _) game =
--   game { pSpeed = ((fst (pSpeed game)), (snd  (pSpeed game)) + speed) }

-- handleKeys (EventKey (Char 'a') Up _ _) game =
--   game { pSpeed = ((fst  (pSpeed game)) + speed, (snd (pSpeed game))) }

-- handleKeys (EventKey (Char 'a') Down _ _) game =
--   game { pSpeed = ((fst  (pSpeed game)) - speed, (snd (pSpeed game))) }

-- handleKeys (EventKey (Char 's') Down _ _) game =
--   game { pSpeed = ((fst (pSpeed game)), (snd  (pSpeed game)) - speed) }

-- handleKeys (EventKey (Char 's') Up _ _) game =
--   game { pSpeed = ((fst (pSpeed game)), (snd  (pSpeed game)) + speed) }

-- handleKeys (EventKey (Char 'd') Down  _ _) game =
--   game { pSpeed = ((fst (pSpeed game)) + speed, (snd  (pSpeed game)) ) }

-- handleKeys (EventKey (Char 'd') Up _ _) game =
--   game { pSpeed = ((fst (pSpeed game)) - speed, (snd  (pSpeed game)) ) }

-- Do nothing for all other events.
handleKeys _ game = game



movePlayerLeft :: GameState -> GameState
movePlayerLeft game = game
  {
    player1 = move (player1 game)

  }
  where 
      move player1 = player1 
        {

          pSpeedx = ( -10)
        }


movePlayerRight :: GameState -> GameState
movePlayerRight game = game
  {
    player1 = move (player1 game)

  }
  where 
      move player1 = player1 {
      pSpeedx = 10}

-- stopPlayer :: GameState -> GameState
-- stopPlayer game = game
--   {
--     player1 = stop(player1  game)

--   }
--   where 
--     stop player = player{
    
--   }

-- -- stopPlayer :: GameState -> GameState
-- -- stopPlayer game = game
-- --   {
-- --       player1 = stop (player1 game )
-- --   }
-- --     where 
-- --         stop player1 = player1 {
-- --         pSpeedx = 0,
-- --         pSpeedy = 0}

-- -- | Остановить игрока.
-- stopPlayer :: Universe -> Universe
-- stopPlayer u = u
--   { universePlayer = bump (universePlayer u)
--   }
--   where
--     bump player = player {
--     playerSpeed = 0}


-- bumpPlayerLeft :: Universe -> Universe
-- bumpPlayerLeft u = u
--   { universePlayer = bump (universePlayer u)
--   }
--   where
--     bump player = player {
--     playerSpeed = -bumpSpeed }

-- -- | Сдвинуть игрока вверх.
-- bumpPlayerUp :: Universe -> Universe
-- bumpPlayerUp u = u
--   { universePlayer = bump (universePlayer u)
--   }
--   where
--     bump player = player {
--     playerFallingSpeed = jumpSpeed }

-- -- |Сдвинуть игрока вправо.
-- bumpPlayerRight :: Universe -> Universe
-- bumpPlayerRight u = u
--   { universePlayer = bump (universePlayer u)
--   }
--   where
--     bump player = player {
--     playerSpeed = bumpSpeed }
