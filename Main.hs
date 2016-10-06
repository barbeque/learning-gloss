module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 100

-- | Describes game state
data PongGame = Game {
  ballPosition :: (Float, Float), -- ^ ball x,y loc
  ballVelocity :: (Float, Float), -- ^ ball vx, vy
  leftPlayerY :: Float, -- ^ Y position of left paddle
  rightPlayerY :: Float -- ^ Y position of right paddle, 0 = middle
} deriving Show

initialState :: PongGame
initialState = Game {
  ballPosition = (-10, 30),
  ballVelocity = (1, -3),
  leftPlayerY = 40,
  rightPlayerY = (-80)
}

-- | Game update function.
moveBall ::
  Float ->      -- ^ delta time since last update
  PongGame ->   -- ^ game state before update
  PongGame      -- ^ updated game state
moveBall t game =
  game { ballPosition = (x', y') }
  where
    (x, y) = ballPosition game
    (vx, vy) = ballVelocity game
    -- New locations
    x' = x + vx * t
    y' = y + vy * t

-- | Draw the current state of the game
render :: PongGame -> Picture
render game = Pictures
          [
            ball, walls,
            paddle rose 120 (leftPlayerY game),
            paddle orange (-120) (rightPlayerY game)
          ]
          where
            -- pong ball
            ball = uncurry translate (ballPosition game) $ color ballColor $ circleSolid 10
            ballColor = dark red

            -- bottom, top walls
            wall :: Float -> Picture
            wall offset = translate 0 offset $ color wallColor $ rectangleSolid 270 10
            wallColor = greyN 0.5
            walls = Pictures [wall 150, wall (-150)]

            paddle :: Color -> Float -> Float -> Picture
            paddle colour x y = pictures
                    [
                      translate x y $ color colour $ rectangleSolid 26 86,
                      translate x y $ color paddleColour $ rectangleSolid 20 80
                    ]
            paddleColour = light (light blue)

{- Return display mode -}
window :: Display
{- InWindow constructor means windowed, FullScreen full -}
window = InWindow "pongle" (width, height) (offset, offset)

{- Return background colour -}
background :: Color
background = black

main :: IO ()
{- Display expects a display mode, background colour, and something to draw... -}
{- Drawing is actually a function that produces a drawing for a float -}
main = animate window background frame
  where
    frame :: Float -> Picture
    frame seconds = render $ moveBall seconds initialState
