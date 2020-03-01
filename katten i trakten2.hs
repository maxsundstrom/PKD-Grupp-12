module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

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
    , eHP :: Int
    , einarHP :: Int
    , turn :: Bool
    , randomGen :: StdGen
    } 

main :: IO ()
main = play window white 60 initial render inputHandler updateFunc


render :: EinarGame -> Picture
render game
    | state game == "menu" = pictures start
    | state game == "game" = pictures einar
    | state game == "fight" = pictures fight
    | otherwise = pictures []
        where
            start = [translate (-650) 0 (scale 0.45 0.5 (text "Play Game = p | Quit game = escape"))]
            einar = [translate (einarX game) (einarY game) (einarSprite), translate schonoSpriteX schonoSpriteY (schonoSprite) , translate (-500) 200 (ainaCarSprite), translate (-500) (0) (ainaSprite)]    
            fight = [translate 100 100 $ text (show (eHP game)), translate 100 250 $ text (show (einarHP game)), translate (-100) 0 einarSprite]

initial :: EinarGame
initial = Game
    { state = "menu"
    , einarX = 0
    , einarY = 0
    , dDown = False
    , aDown = False
    , sDown = False
    , wDown = False
    , eHP = 10
    , einarHP = 20
    , turn = True
    , randomGen = mkStdGen 1
    }

schonoSpriteX :: Float
schonoSpriteX = 325
schonoSpriteY :: Float
schonoSpriteY = 200

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
inputHandler (EventKey (SpecialKey keySpace) Down _ _) game
  | (state game) == "fight" = fight game

inputHandler (EventKey (Char 'p') down _ _) game 
    | (state game) == "menu" = game { state = "game" }
inputHandler _ game = game

updateFunc :: Float -> EinarGame -> EinarGame
updateFunc w game

  | (state game) == "game" && abs((einarX game) - schonoSpriteX) < 50 && abs((einarY game) - schonoSpriteY) < 50 = game {state = "fight", eHP = 10, einarHP = 20}
  | (state game) == "game" && (dDown game) = game { einarX = (einarX game) + 3 }
  | (state game) == "game" && (aDown game) = game { einarX = (einarX game) - 3 }
  | (state game) == "game" && (sDown game) = game { einarY = (einarY game) - 3 }
  | (state game) == "game" && (wDown game) = game { einarY = (einarY game) + 3 }
  | otherwise = game

fight :: EinarGame -> EinarGame
fight game
    | (einarHP game) <= 0 = game {state = "menu", einarX = 0, einarY = 0}
    | (eHP game) <= 0 = game {state = "game", einarX = 0, einarY = 0}
    | (turn game) = 
      let (gen1, gen2) = split (randomGen game)
      in game { einarHP = (einarHP game) - (fst(randomR (1,5) (gen1))), eHP = (eHP game) - (fst(randomR (1,3) gen2)), randomGen = gen2}

background :: Color
background = white

laddbackground :: Color
laddbackground = black

ainaCarSprite :: Picture
ainaCarSprite = pictures 
    [ translate (-4) (40) (color black (rectangleSolid 3 126))   -- Car left side
    , translate (120) (101) (color black (rectangleSolid 250 3)) -- Car top
    , translate (120) (-23) (color black (rectangleSolid 250 3)) -- Car bottom
    , translate (245) (40) (color black (rectangleSolid 3 126))  -- Car right side
    , translate (30) (-20) (color black (circleSolid 20))        -- Back wheel
    , translate (212) (-20) (color black (circleSolid 20))       -- Front wheel
    , translate 60 25 (color black (scale 0.3 0.3 (text "Polis")))
    ]

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

laddSprite :: Picture
laddSprite = pictures
  [translate (-4) (-40) (color white (rectangleSolid 70 20))                           -- white rectangle
  , translate (-29) (-40) (color (makeColorI 224 218 144 255) (rectangleSolid 30 20))  -- Beige rectangle
  ]

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