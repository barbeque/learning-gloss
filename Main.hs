module Main(main) where

import Graphics.Gloss

{- Return display mode -}
window :: Display
{- InWindow constructor means windowed, FullScreen full -}
window = InWindow "Nice Window" (200, 200) (10, 10)

{- Return background colour -}
background :: Color
background = white

{- Return what to draw -}
drawing :: Picture
drawing = translate 10 10 (circle 80) {- circle, radius 80 -}

main :: IO ()
{- Display expects a display mode, background colour, and something to draw... -}
{- Drawing is actually a function that produces a drawing for a float -}
main = display window background drawing
