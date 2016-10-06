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
          [ translate (-20) (-100) $ color ballColor $ circleSolid 30,
            translate 30 50 $ color paddleColor $ rectangleSolid 10 50
          ]
          where
            ballColor = dark red
            paddleColor = light (light blue)

main :: IO ()
{- Display expects a display mode, background colour, and something to draw... -}
{- Drawing is actually a function that produces a drawing for a float -}
main = display window background drawing
