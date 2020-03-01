module Einar(einar) where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


{- Renders the picture on a fullscreen -}
window :: Display
window = FullScreen


data EinarGame = Game
    { state :: String
    , einarX :: Float
    , einarY :: Float
    , dDown :: Bool
    , aDown :: Bool
    , sDown :: Bool
    , wDown :: Bool
    ,currenDia :: String
    ,nextDia :: [String]
    } deriving Show

{-The function that runs the game with the help of all the other functions -}
main :: IO ()
main = play window white 60 initial render inputHandler updateFunc

{-Renders a appropriate picture depending on what state the game is currently -}
render :: EinarGame -> Picture
render game
    | state game == "menu" = pictures start
    | state game == "game" = pictures einar
    | state game == "talk" = pictures ainahead
        where
            start = [translate (-650) 0 (scale 0.45 0.5 (text "Play Game = p | Quit game = escape"))]
            einar = [ translate 325 200 (schonoSprite), translate (-500) (0) (ainaSprite) ,translate (einarX game) (einarY game) (einarSprite)]
            ainahead = [translate 0 0 (ainaSprite), translate 50 0 (einarSprite), translate 0 (-25) (talkBubbleSprite game)] 

{-The initial state of the game-}
initial :: EinarGame
initial = Game
    { state = "menu"
    , einarX = 0
    , einarY = 0
    , dDown = False
    , aDown = False
    , sDown = False
    , wDown = False
    , currenDia = head diapolis1
    , nextDia = tail diapolis1
    }

{- Changes the state of the game depending on different inputs -}
inputHandler :: Event -> EinarGame -> EinarGame
inputHandler (EventKey (Char 'd') keyState _ _) game
  | (state game) == "game" && keyState == Down = game { dDown = True }
  | (state game) == "game" && keyState == Up = game { dDown = False }
inputHandler (EventKey (Char 's') keyState _ _) game
  | (state game) == "game" && keyState == Down = game { sDown = True }
  | (state game) == "game" && keyState == Up = game { sDown = False }
inputHandler (EventKey (Char 'w') keyState _ _) game
  | (state game) == "game" && keyState == Down = game { wDown = True }
  | (state game) == "game" && keyState == Up = game { wDown = False }
inputHandler (EventKey (Char 'a') keyState _ _) game
  | (state game) == "game" && keyState == Down = game { aDown = True }
  | (state game) == "game" && keyState == Up = game { aDown = False }

inputHandler (EventKey (Char 'p') Down _ _ ) game | (state game) == "menu" = game { state = "game" }
inputHandler (EventKey (Char 't') Down _ _ ) game | (state game) == "talk" = talkingfunc game
inputHandler _ game = game

{- Changes the cordinates of einar which makes him move -}
updateFunc :: Float -> EinarGame -> EinarGame
updateFunc w game
  | (state game) == "game" && (dDown game) = game { einarX = (einarX game) + 2 }
  | (state game) == "game" && (aDown game) = game { einarX = (einarX game) - 2 }
  | (state game) == "game" && (sDown game) = game { einarY = (einarY game) - 2 }
  | (state game) == "game" && (wDown game) = game { einarY = (einarY game) + 2 }
  | (state game) == "game" && abs((einarX game) - ainaSpriteX) < 50 && abs((einarY game) - ainaSpriteY) < 50 = game {state = "talk"}
  | otherwise = game


{- Keeps track of the x cordinate where the aina is positioned-}
ainaSpriteX :: Float
ainaSpriteX  = (-500)

{- Keeps track of the y cordinate where the aina is positioned -}
ainaSpriteY :: Float
ainaSpriteY = 0  

{- The picture of the police car -}
ainaCarSprite :: Picture
ainaCarSprite = pictures 
    [ translate (-4) (40) (color black (rectangleSolid 3 126))     -- Car left side
    , translate (120) (101) (color black (rectangleSolid 250 3))   -- Car top
    , translate (120) (-23) (color black (rectangleSolid 250 3))   -- Car bottom
    , translate (245) (40) (color black (rectangleSolid 3 126))    -- Car right side
    , translate (30) (-20) (color black (circleSolid 20))          -- Back wheel
    , translate (212) (-20) (color black (circleSolid 20))         -- Front wheel
    , translate 60 25 (color black (scale 0.3 0.3 (text "Polis"))) -- Police text
    ]

{- The picture of the police man ("aina") -}
ainaSprite :: Picture
ainaSprite = pictures 
    [ translate (-4) (-40) (color (makeColorI 43 161 204 255) (rectangleSolid 20 60))   -- Body light blue
  , translate (-4) (-10) (color (makeColorI 255 173 201 255) (circleSolid 15))          -- Head
  , color (makeColorI 2 11 64 255) (rectangleSolid 40 10)                               -- Hat bottom dark blue
  , translate (-5) 10 (color (makeColorI 2 11 64 255) (rectangleSolid 30 20))           -- Hat top
  , translate (-18) 5 (color white (scale 0.08 0.1 (text "Aina")))                      -- Hat text
  , translate (-4) (-60) (color (makeColorI 2 11 64 255) (rectangleSolid 20 15))        -- Pants
  , translate   1 (-45) (color (makeColorI 2 11 64 255) (rectangleSolid 5 40))          -- Pants 2
  , translate (-7) (-45) (color (makeColorI 2 11 64 255) (rectangleSolid 5 40))         -- Pants 3
  , translate (-6) (-45) (color (makeColorI 255 173 201 255) (circleSolid 5))           -- Hands
  ]

{- The picture of Einar -}
einarSprite :: Picture
einarSprite = pictures
  [ translate (-4) (-40) (color black (rectangleSolid 20 60))                        -- Body
  , translate (-4) (-10) (color (makeColorI 255 173 201 255) (circleSolid 15))       -- Head
  , color orange (rectangleSolid 40 10)                                              -- Hat bottom
  , translate (-5) 10 (color orange (rectangleSolid 30 20))                          -- Hat top
  , translate (-18) 5 (scale 0.08 0.1 (text "Gucci"))                                -- Hat text
  , translate (-4) (-60)(color blue (rectangleSolid 20 15))                          -- Pants
  , translate (-6) (-45) (color (makeColorI 255 173 201 255) (circleSolid 5))        -- Hand
  ] 

{- The picture of Schono -}
schonoSprite :: Picture
schonoSprite = pictures
  [ translate (-4) (-40) (color green (rectangleSolid 20 60))                        -- Body
  , translate (-4) (-10) (color (makeColorI 255 173 201 255) (circleSolid 15))       -- Head
  , color black (rectangleSolid 40 10)                                               -- Hat bottom
  , translate (-5) 10 (color black (rectangleSolid 30 20))                           -- Hat top
  , translate (-6) 5 (color green (scale 0.08 0.1 (text "M")))                       -- Hat text
  , translate (-4) (-60)(color (makeColorI 224 218 144 255) (rectangleSolid 20 15))  -- Pants
  , translate (-6) (-45) (color (makeColorI 255 173 201 255) (circleSolid 5))        -- Hand
  ]

{- The picture of the talking bubbles-}
talkBubbleSprite :: EinarGame -> Picture
talkBubbleSprite game = pictures  
  [ translate (-540) (-295) (color black (rectangleSolid 3 180))                     -- Left boarder line for chat bubble
  , translate (540) (-295) (color black (rectangleSolid 3 180))                      -- Right boarder line for chat bubble
  , translate 0 (-205) (color black (rectangleSolid 1080 3))                         -- Top boarder line for chat bubble
  , translate (-50) (-325) (color black (scale 0.4 0.4 (text (currenDia game))))      -- The dialogue in the chat bubble.
  ]



talkingfunc :: EinarGame -> EinarGame
talkingfunc game 
  | (nextDia game) == [] = game {state = "menu"}
  | otherwise = game {currenDia = head (nextDia game), nextDia = tail (nextDia game)}

diapolis1 = ["hej","på","dig"]

background :: Color
background = white

schono :: IO ()
schono = display window background schonoSprite

einar :: IO ()
einar = display window background einarSprite

aina :: IO ()
aina = display window background ainaSprite

car :: IO ()
car = display window background ainaCarSprite