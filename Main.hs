module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 100

{- Return display mode -}
window :: Display
{- InWindow constructor means windowed, FullScreen full -}
window = InWindow "pongle" (width, height) (offset, offset)

{- Return background colour -}
background :: Color
background = black

{- Return what to draw -}
drawing :: Picture
drawing = Pictures
          [
            ball, walls,
            paddle rose 120 (-20),
            paddle orange (-120) 40
          ]
          where
            -- pong ball
            ball = translate (-10) 40 $ color ballColor $ circleSolid 10
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

main :: IO ()
{- Display expects a display mode, background colour, and something to draw... -}
{- Drawing is actually a function that produces a drawing for a float -}
main = display window background drawing
