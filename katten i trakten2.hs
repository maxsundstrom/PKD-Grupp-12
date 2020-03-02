module Einar(main) where

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
    , eHP :: Int 
    , einarHP :: Int
    , turn :: Bool
    , randomGen :: StdGen
    , currenDia :: String
    , nextDia :: [String]
    , direction :: Float -- -1 = facing left  1 = facing right

    } deriving Show

{-The function that runs the game with the help of all the other functions -}
main :: IO ()
main = play window (makeColorI 201 198 193 255) 60 initial render inputHandler updateFunc

{-Renders a appropriate picture depending on what state the game is currently -}
render :: EinarGame -> Picture
render game
    | state game == "menu" = pictures start
    | state game == "game" = pictures einar
    | state game == "fight" = pictures fight
    | state game == "talk" = pictures ainahead
    | state game == "game2" = pictures copcar
    | state game == "talkChief" = pictures talkingchief
    | state game == "fightchief" = pictures fightingchief
    | state game == "endscreen" = pictures end
    | otherwise = pictures []
        where
            start = [translate (-650) 0 (scale 0.45 0.5 (text "Play Game = p | Quit game = escape"))]
            einar = [translate schonoSpriteX schonoSpriteY (schonoSprite), translate (-500) (0) (ainaSprite), translate (einarX game) (einarY game) (einarSprite)]
            ainahead = [translate 0 0 (ainaSprite), translate 50 0 (einarSprite), translate 0 (-25) (talkBubbleSprite game)] 
            fight = [translate 0 (-25) (talkBubbleSprite game), translate 100 100 $ text (show (eHP game))
                    , translate (-150) 100 $ text (show (einarHP game)), translate (-100) 0 einarSprite
                    , translate 100 0 $ scale (-1) 1 $ schonoSprite]
            copcar = [ translate (-500) 200 (ainaCarSprite), translate (-500) 0 (ainaSprite)
                    , translate (-450) 0 (ainaSprite), translate (-400) 0 (ainaSprite)
                    , translate (-475) 100 (ainaSprite), translate (-425) 100 (ainaSprite)
                    , translate (-380) 100 (ainaChief), translate (einarX game) (einarY game) (einarSprite)
                    , translate schonoSpriteX schonoSpriteY (schonoSprite)]
            talkingchief = [translate (-500) 200 (ainaCarSprite), translate (-500) 0 (ainaSprite)
                            , translate (-450) 0 (ainaSprite), translate (-400) 0 (ainaSprite)
                            , translate (-475) 100 (ainaSprite), translate (-425) 100 (ainaSprite)
                            , translate (-380) 100 (ainaChief), translate (-340) 100 (einarSprite)
                            ,translate schonoSpriteX schonoSpriteY (schonoSprite), translate 0 0 (talkBubbleSprite game)]
            fightingchief = [translate 0 (-25) (talkBubbleSprite game), translate (-150) 100 $ text (show (eHP game))
                            , translate 100 100 $ text (show (einarHP game)),translate (-100) 0 (ainaChief)
                            , translate 100 0 (einarSprite)]
            end = [translate (-200) 0 (scale 0.7 0.7 (text "The end"))]

            
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
    , eHP = 10
    , einarHP = 20
    , turn = True
    , randomGen = mkStdGen 1
    , currenDia = []
    , nextDia = []
    }
{- Keeps track of the x cordinate of schono -}    
schonoSpriteX :: Float
schonoSpriteX = 325

{- Keeps track of the y cordinate of schono -}
schonoSpriteY :: Float
schonoSpriteY = 200

{- Controls what happens with the diffrenet inputs depending on what state the game is in -}


inputHandler :: Event -> EinarGame -> EinarGame
inputHandler (EventKey (Char 'd') keyState _ _) game

  | ((state game) == "game" || (state game) == "game2") && keyState == Down = game { dDown = True }
  | ((state game) == "game" || (state game) == "game2") && keyState == Up = game { dDown = False }
inputHandler (EventKey (Char 's') keyState _ _) game
  | ((state game) == "game" || (state game) == "game2") && keyState == Down = game { sDown = True }
  | ((state game) == "game" || (state game) == "game2") && keyState == Up = game { sDown = False }
inputHandler (EventKey (Char 'w') keyState _ _) game
  | ((state game) == "game" || (state game) == "game2") && keyState == Down = game { wDown = True }
  | ((state game) == "game" || (state game) == "game2") && keyState == Up = game { wDown = False }
inputHandler (EventKey (Char 'a') keyState _ _) game
  | ((state game) == "game" || (state game) == "game2") && keyState == Down = game { aDown = True }
  | ((state game) == "game" || (state game) == "game2") && keyState == Up = game { aDown = False }

inputHandler (EventKey (Char 'p') Down _ _ ) game | (state game) == "menu" = game { state = "game" }
inputHandler (EventKey (SpecialKey keySpace) Down _ _ ) game | (state game) == "talk" = talkingfunc game
inputHandler (EventKey (SpecialKey keySpace) Down _ _ ) game | (state game) == "talkChief" = talkingfunc2 game

inputHandler (EventKey (SpecialKey keySpace) Down _ _) game | (state game) == "fight" = fight game
inputHandler (EventKey (SpecialKey keySpace) Down _ _) game | (state game) == "fightchief" = fight2 game

inputHandler (EventKey (Char 'p') down _ _) game | (state game) == "menu" = game { state = "game" }


inputHandler _ game = game

updateFunc :: Float -> EinarGame -> EinarGame
updateFunc w game  
{- Changes the cordinates of einar which makes him move -}
  | (state game) == "game" && abs((einarX game) - schonoSpriteX) < 50 && abs((einarY game) - schonoSpriteY) < 50 = game {state = "fight", eHP = 10, einarHP = 20}
  | (state game) == "game" && abs((einarX game) - ainaSpriteX) < 50 && abs((einarY game) - ainaSpriteY) < 50 = game {state = "talk", currenDia = head (diapolis1), nextDia = tail (diapolis1)}
  | (state game) == "game2" && abs((einarX game) - ainaChiefX) < 50 && abs((einarY game) - ainaChiefY) < 50 = game {state = "talkChief", currenDia = head (diaChief), nextDia = tail (diaChief)}
  | ((state game) == "game" || (state game) == "game2") && (dDown game) = game { einarX = (einarX game) + 3 }
  | ((state game) == "game" || (state game) == "game2") && (aDown game) = game { einarX = (einarX game) - 3 }
  | ((state game) == "game" || (state game) == "game2") && (sDown game) = game { einarY = (einarY game) - 3 }
  | ((state game) == "game" || (state game) == "game2") && (wDown game) = game { einarY = (einarY game) + 3 }
  | otherwise = game

fight :: EinarGame -> EinarGame
fight game
    | (currenDia game) == "Einar defeated his opponent" = game {state = "game2", einarX = 0, einarY = 0, dDown = False, wDown = False, sDown = False, aDown = False}
    | (einarHP game) <= 0 = game {state = "menu", einarX = 0, einarY = 0}
    | (eHP game) <= 0 = game {currenDia = "Einar defeated his opponent"} 
    | (turn game) = 
      let (gen1, gen2) = split (randomGen game)
      in game {einarHP = (einarHP game) - (fst(randomR (1,5) (gen1))), eHP = (eHP game) - (fst(randomR (1,3) gen2)), randomGen = gen2, currenDia = "Einar recieves " ++ (show $ fst $ (randomR (1,3) gen2 :: (Int, StdGen))) ++ " damage \n Einar deals " ++ (show $ fst $ (randomR (1,5) gen1 :: (Int, StdGen))) ++ " damage" }


fight2 :: EinarGame -> EinarGame
fight2 game
    | (currenDia game) == "Einar defeated his opponent" = game {state = "endscreen"}
    | (einarHP game) <= 0 = game {state = "menu", einarX = 0, einarY = 0}
    | (eHP game) <= 0 = game {currenDia = "Einar defeated his opponent"} 
    | (turn game) = 
      let (gen1, gen2) = split (randomGen game)
      in game {einarHP = (einarHP game) - (fst(randomR (1,5) (gen1))), eHP = (eHP game) - (fst(randomR (1,3) gen2)), randomGen = gen2, currenDia = "Einar recieves " ++ (show $ fst $ (randomR (1,3) gen2 :: (Int, StdGen))) ++ " damage \n Einar deals " ++ (show $ fst $ (randomR (1,5) gen1 :: (Int, StdGen))) ++ " damage" }

ainaChief :: Picture
ainaChief = pictures 
  [ translate (-4) (-40) (color (makeColorI 43 161 204 255) (rectangleSolid 20 60))   -- Body light blue
  , translate (-4) (-10) (color (makeColorI 255 173 201 255) (circleSolid 15))        -- Head
  , color (makeColorI 2 11 64 255) (rectangleSolid 40 10)                             -- Hat bottom dark blue
  , translate (-5) 10 (color (makeColorI 2 11 64 255) (rectangleSolid 30 20))         -- Hat top
  , translate (-18) 5 (color white (scale 0.08 0.1 (text "Chief")))                   -- Hat text
  , translate (-4) (-60) (color (makeColorI 2 11 64 255) (rectangleSolid 20 15))      -- Pants
  , translate   1 (-45) (color (makeColorI 2 11 64 255) (rectangleSolid 5 40))        -- Pants 2
  , translate (-7) (-45) (color (makeColorI 2 11 64 255) (rectangleSolid 5 40))       -- Pants 3
  , translate (-6) (-45) (color (makeColorI 255 173 201 255) (circleSolid 5))         -- Hands
  , translate (-3) (-30) (color yellow (circleSolid 3))]                              -- Badge


{- The picture of the police car -}
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

{- The picture of the police man ("aina") -}
ainaSprite :: Picture
ainaSprite = pictures 
  [ translate (-4) (-40) (color (makeColorI 43 161 204 255) (rectangleSolid 20 60))   -- Body light blue
  , translate (-4) (-10) (color (makeColorI 255 173 201 255) (circleSolid 15))        -- Head
  , color (makeColorI 2 11 64 255) (rectangleSolid 40 10)                             -- Hat bottom dark blue
  , translate (-5) 10 (color (makeColorI 2 11 64 255) (rectangleSolid 30 20))         -- Hat top
  , translate (-18) 5 (color white (scale 0.08 0.1 (text "Aina")))                    -- Hat text
  , translate (-4) (-60) (color (makeColorI 2 11 64 255) (rectangleSolid 20 15))      -- Pants
  , translate   1 (-45) (color (makeColorI 2 11 64 255) (rectangleSolid 5 40))        -- Pants 2
  , translate (-7) (-45) (color (makeColorI 2 11 64 255) (rectangleSolid 5 40))       -- Pants 3
  , translate (-6) (-45) (color (makeColorI 255 173 201 255) (circleSolid 5))         -- Hands
  ]
{- Keeps track of the x cordinate where the aina is positioned-}
ainaSpriteX :: Float
ainaSpriteX  = (-500)

{- Keeps track of the y cordinate where the aina is positioned -}
ainaSpriteY :: Float

ainaSpriteY = 0

{- Keeps track of the x cordinate where ainaChief is positioned -}
ainaChiefX :: Float
ainaChiefX = (-400)

{- Keeps track of the y cordinate where ainaChief is positioned -}
ainaChiefY :: Float
ainaChiefY = 125 


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
  , translate (-500) (-325) (color black (scale 0.3 0.4 (text (currenDia game))))    -- The dialogue in the chat bubble.

  ]

{- Walks throught the list element by element and outputs a game state depending on the element -}
talkingfunc :: EinarGame -> EinarGame
--Variant: Lenght of the list
talkingfunc game 
  | (nextDia game) == [] = game {state = "game2", einarY = 0, einarX = 0}
  | otherwise = game {currenDia = head (nextDia game), nextDia = tail (nextDia game)}
{- Walks throught the list element by element and outputs a game state depending on the element. -}
talkingfunc2 :: EinarGame -> EinarGame
--Variant: Lenght of the list
talkingfunc2 game 
  | (nextDia game) == [] = game {state = "fightchief", eHP = 10, einarHP = 20}
  | otherwise = game {currenDia = head (nextDia game), nextDia = tail (nextDia game)}


diapolis1 = ["Einar: Sho bre","Aina: Good day sir","Einar: You got some chine white??","Aina: You trying to be funny huh?","Aina: Watch out so i don't shoot you"]

diaChief = ["Einar: China white?", "Aina: Mr Einar you're under arrest", "Einar: You wont take me alive!"]


background :: Color
background = white


