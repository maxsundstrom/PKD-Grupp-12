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
    , direction :: Float -- 1 = facing right  -1 = facing left
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
    | state game == "talkSchono" = pictures talkingSchono
    | state game == "endscreen" = pictures end
    | otherwise = pictures []
        where
            start = [translate (-650) 0 (scale 0.45 0.5 (text "Play Game = p | Quit game = escape")), translate (-325) 200 (scale 0.45 0.5 (text "Einar the game"))]

            einar = [translate 0 0 (streetSprite),translate schonoSpriteX schonoSpriteY (schonoSprite), translate (-500) (0) (ainaSprite), translate (einarX game) (einarY game) $ scale (direction game) 1 $ (einarSprite)]
            ainahead = [translate 0 0 (streetSprite) ,translate 0 0 (ainaSprite), translate 50 0 $ scale (-1) 1 $ (einarSprite), translate 0 (-25) (talkBubbleSprite game)] 
            fight = [translate 0 0 (streetSprite) ,translate 0 (-25) (talkBubbleSprite game), translate 100 100 $ text (show (eHP game))
                    , translate (-150) 100 $ text (show (einarHP game)), translate (-100) 0 einarSprite
                    , translate 100 0 $ scale (-1) 1 $ schonoSprite]
            copcar = [ translate 0 0 (streetSprite) ,translate (-500) (-175) (ainaCarSprite), translate (-500) 0 (ainaSprite)
                    , translate (-450) 0 (ainaSprite), translate (-400) 0 (ainaSprite)
                    , translate (-475) 100 (ainaSprite), translate (-425) 100 (ainaSprite)
                    , translate (-380) 100 (ainaChief), translate (einarX game) (einarY game) $ scale (direction game) 1 $ (einarSprite)]
            talkingchief = [translate 0 0 (streetSprite),translate (-500) (-175) (ainaCarSprite), translate (-500) 0 (ainaSprite)


                            , translate (-450) 0 (ainaSprite), translate (-400) 0 (ainaSprite)
                            , translate (-475) 100 (ainaSprite), translate (-425) 100 (ainaSprite)
                            , translate (-380) 100 (ainaChief), translate (-340) 100 $ scale (-1) 1 $ (einarSprite)
                            , translate 0 0 (talkBubbleSprite game)]

            fightingchief = [translate 0 0 (streetSprite) ,translate 0 (-25) (talkBubbleSprite game), translate (-150) 100 $ text (show (eHP game))
                            , translate 100 100 $ text (show (einarHP game)), translate (-100) 0 (ainaChief)
                            , translate 100 0 $ scale (-1) 1 $ (einarSprite), translate (-500) (-175) (ainaCarSprite), translate (-500) 0 (ainaSprite)
                            , translate (-450) 0 (ainaSprite), translate (-400) 0 (ainaSprite)
                            , translate (-475) 100 (ainaSprite), translate (-425) 100 (ainaSprite)]
            talkingSchono = [ translate 0 0 (streetSprite), translate 0 (-25) (talkBubbleSprite game)
                            , translate (-100) 0 einarSprite
                            , translate 100 0 $ scale (-1) 1 $ schonoSprite]
            end = [translate 0 0 (streetSprite) ,translate (-200) 0 (scale 0.7 0.7 (text "The end"))] 



            
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
    , direction = 1
    }
{- Keeps track of the x cordinate of schono -}    
schonoSpriteX :: Float
schonoSpriteX = 325

{- Keeps track of the y cordinate of schono -}
schonoSpriteY :: Float

schonoSpriteY = (-100)


{- Controls what happens with the diffrenet inputs depending on what state the game is in -}
inputHandler :: Event -> EinarGame -> EinarGame
-- Used to know when one of the moving keys is pressed down or not
inputHandler (EventKey (Char 'd') keyState _ _) game
  | ((state game) == "game" || (state game) == "game2") && keyState == Down = game { dDown = True, direction = 1 }
  | ((state game) == "game" || (state game) == "game2") && keyState == Up = game { dDown = False }
inputHandler (EventKey (Char 's') keyState _ _) game
  | ((state game) == "game" || (state game) == "game2") && keyState == Down = game { sDown = True }
  | ((state game) == "game" || (state game) == "game2") && keyState == Up = game { sDown = False }
inputHandler (EventKey (Char 'w') keyState _ _) game
  | ((state game) == "game" || (state game) == "game2") && keyState == Down = game { wDown = True }
  | ((state game) == "game" || (state game) == "game2") && keyState == Up = game { wDown = False }
inputHandler (EventKey (Char 'a') keyState _ _) game
  | ((state game) == "game" || (state game) == "game2") && keyState == Down = game { aDown = True, direction = (-1)  }
  | ((state game) == "game" || (state game) == "game2") && keyState == Up = game { aDown = False }

-- When p is pressed exit the main menu and go to the game state.
inputHandler (EventKey (Char 'p') Down _ _ ) game | (state game) == "menu" = game { state = "game" }

-- Space is used to progress talking and fighting in all stages of the game
inputHandler (EventKey (SpecialKey keySpace) Down _ _ ) game | (state game) == "talk" = talkingfunc game
                                                             | (state game) == "talkChief" = talkingfunc2 game
                                                             | (state game) == "talkSchono" = talkingfunc3 game
                                                             | (state game) == "fight" = fight game
                                                             | (state game) == "fightchief" = fight2 game
inputHandler _ game = game

{- Changes the cordinates of einar which makes him move and handles what happens when you come close to certain characters -}
updateFunc :: Float -> EinarGame -> EinarGame
updateFunc w game 
-- Enables the talking sequence with shono
  | (state game) == "game" && abs((einarX game) - schonoSpriteX) < 50 && abs((einarY game) - schonoSpriteY) < 50 = game {state = "talkSchono", currenDia = head (diaSchono), nextDia = tail (diaSchono)}
  -- Enables the talking sequence with aina
  | (state game) == "game" && abs((einarX game) - ainaSpriteX) < 50 && abs((einarY game) - ainaSpriteY) < 50 = game {state = "talk", currenDia = head (diapolis1), nextDia = tail (diapolis1)}
  -- Enables the talking sequence with the police chief
  | (state game) == "game2" && abs((einarX game) - ainaChiefX) < 50 && abs((einarY game) - ainaChiefY) < 50 = game {state = "talkChief", currenDia = head (diaChief), nextDia = tail (diaChief)}
  -- Enables einars movement
  | ((state game) == "game" || (state game) == "game2") && (dDown game) = game { einarX = (einarX game) + 3 }
  | ((state game) == "game" || (state game) == "game2") && (aDown game) = game { einarX = (einarX game) - 3 }
  | ((state game) == "game" || (state game) == "game2") && (sDown game) = game { einarY = (einarY game) - 3 }
  | ((state game) == "game" || (state game) == "game2") && (wDown game) = game { einarY = (einarY game) + 3 }
  | otherwise = game

fight :: EinarGame -> EinarGame
fight game
-- Checks if opponent deafeated
    | (currenDia game) == "Einar defeated his opponent" = game {state = "game2", einarX = 0, einarY = 0, dDown = False, wDown = False, sDown = False, aDown = False}
    -- If einars hp is reduced to zero
    | (einarHP game) <= 0 = game {state = "menu", einarX = 0, einarY = 0}
    -- If enemy hp is reduced to zero
    | (eHP game) <= 0 = game {currenDia = "Einar defeated his opponent"} 
    | otherwise = 
      -- Generates two new StdGen from the current one
      let (gen1, gen2) = split (randomGen game)
      -- Removes random amount from einars hp and opponents hp
      in game {einarHP = (einarHP game) - (fst(randomR (1,5) (gen1))), eHP = (eHP game) - (fst(randomR (1,3) gen2)), randomGen = gen2, currenDia = "Einar recieves " ++ (show $ fst $ (randomR (1,3) gen2 :: (Int, StdGen))) ++ " damage \n Einar deals " ++ (show $ fst $ (randomR (1,5) gen1 :: (Int, StdGen))) ++ " damage" }

fight2 :: EinarGame -> EinarGame
fight2 game
-- Checks if opponent deafeated
    | (currenDia game) == "Einar defeated his opponent" = game {state = "endscreen"}
    -- If einars hp is reduced to zero
    | (einarHP game) <= 0 = game {state = "menu", einarX = 0, einarY = 0}
    -- If enemy hp is reduced to zero
    | (eHP game) <= 0 = game {currenDia = "Einar defeated his opponent"} 
    | otherwise = 
      -- Removes random amount from einars hp and opponents hp
      let (gen1, gen2) = split (randomGen game)
      -- Removes random amount from einars hp and opponents hp
      in game {einarHP = (einarHP game) - (fst(randomR (1,5) (gen1))), eHP = (eHP game) - (fst(randomR (1,3) gen2)), randomGen = gen2, currenDia = "Einar recieves " ++ (show $ fst $ (randomR (1,3) gen2 :: (Int, StdGen))) ++ " damage \n Einar deals " ++ (show $ fst $ (randomR (1,5) gen1 :: (Int, StdGen))) ++ " damage" }


streetSprite :: Picture
streetSprite = pictures 
    [ translate (0) (350) (color black (rectangleSolid 1600 250)) -- asphalt
    , translate (0) (220) (color (makeColorI 46 49 49 255) (rectangleSolid 1600 20)) -- street edge

    , translate (400) (25) (color (makeColorI 150 40 27 255) (rectangleSolid 300 75)) -- garden
    , translate (300) (150) (color (makeColorI 211 84 0 255) (rectangleSolid 35 250)) --tree1 trunk
    , translate (500) (150) (color (makeColorI 211 84 0 255) (rectangleSolid 35 250)) --tree2 trunk
    , translate (250) (200) (color (makeColorI 0 230 64 255) (circleSolid 40)) --tree1
    , translate (300) (200) (color (makeColorI 0 230 64 255) (circleSolid 40)) --tree1
    , translate (350) (200) (color (makeColorI 0 230 64 255) (circleSolid 40)) --tree1
    , translate (275) (250) (color (makeColorI 0 230 64 255) (circleSolid 40)) --tree1
    , translate (325) (250) (color (makeColorI 0 230 64 255) (circleSolid 40)) --tree1
    , translate (450) (200) (color (makeColorI 0 230 64 255) (circleSolid 40)) --tree2
    , translate (500) (200) (color (makeColorI 0 230 64 255) (circleSolid 40)) --tree2
    , translate (550) (200) (color (makeColorI 0 230 64 255) (circleSolid 40)) --tree2
    , translate (475) (250) (color (makeColorI 0 230 64 255) (circleSolid 40)) --tree2
    , translate (525) (250) (color (makeColorI 0 230 64 255) (circleSolid 40)) --tree2 
    , translate (0) (375) (color white (rectangleSolid 100 3)) -- street line
    , translate (300) (375) (color white (rectangleSolid 100 3)) -- street line
    , translate (600) (375) (color white (rectangleSolid 100 3)) -- street line
    , translate (-300) (375) (color white (rectangleSolid 100 3)) -- street line
    , translate (-600) (375) (color white (rectangleSolid 100 3)) -- street line
    , translate (-350) (-400) (color (makeColorI 108 122 137 255) (rectangleSolid 1000 75)) -- house 1
    , translate (550) (-400) (color (makeColorI 108 122 137 255) (rectangleSolid 450 75)) -- house 2
    , translate (0) (200) (color (makeColorI 108 122 137 255) (rectangleSolid 10 200)) -- lightpost 1
    , translate (0) (300) (color (makeColorI 108 122 137 255) (rectangleSolid 25 25)) -- lightpost 1
    , translate (15) (300) (color (makeColorI 238 238 0 255) (rectangleSolid 10 25)) -- lightbulb 1
    , translate (-400) (200) (color (makeColorI 108 122 137 255) (rectangleSolid 10 200)) -- lightpost 2
    , translate (-400) (300) (color (makeColorI 108 122 137 255) (rectangleSolid 25 25)) -- lightpost 2
    , translate (-385) (300) (color (makeColorI 238 238 0 255) (rectangleSolid 10 25)) -- lightbulb 2
    , translate (400) (200) (color (makeColorI 108 122 137 255) (rectangleSolid 10 200)) -- lightpost 3
    , translate (400) (300) (color (makeColorI 108 122 137 255) (rectangleSolid 25 25)) -- lightpost 3
    , translate (415) (300) (color (makeColorI 238 238 0 255) (rectangleSolid 10 25)) -- lightbulb 3
    ] 


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
    [ translate (-4) (40) (color black (rectangleSolid 3 126))     -- Car left side
    , translate (120) (101) (color black (rectangleSolid 250 3))   -- Car top
    , translate (120) (-23) (color black (rectangleSolid 250 3))   -- Car bottom
    , translate (245) (40) (color black (rectangleSolid 3 126))    -- Car right side
    , translate (30) (-20) (color black (circleSolid 20))          -- Back wheel
    , translate (212) (-20) (color black (circleSolid 20))         -- Front wheel
    , translate 60 25 (color black (scale 0.3 0.3 (text "Polis"))) -- Polis text
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
--Variant: Lenght of diapolis1
talkingfunc game 
  | (nextDia game) == [] = game {state = "game2", einarY = 0, einarX = 0, dDown = False, wDown = False, sDown = False, aDown = False}
  | otherwise = game {currenDia = head (nextDia game), nextDia = tail (nextDia game)}

{- Walks throught the list element by element and outputs a game state depending on the element. -}
talkingfunc2 :: EinarGame -> EinarGame
--Variant: Lenght of diaChief
talkingfunc2 game 
  | (nextDia game) == [] = game {state = "fightchief", eHP = 10, einarHP = 20}
  | otherwise = game {currenDia = head (nextDia game), nextDia = tail (nextDia game)}

{- Walks throught the list element by element and outputs a game state depending on the element. -}
talkingfunc3 :: EinarGame -> EinarGame
--Variant: Lenght of diaschono
talkingfunc3 game 
  | (nextDia game) == [] = game {state = "fight", eHP = 7, einarHP = 20}
  | otherwise = game {currenDia = head (nextDia game), nextDia = tail (nextDia game)}

diapolis1 = ["Einar: Sho bre","Aina: Good day sir","Einar: You got some china white??","Aina: You trying to be funny huh?","Aina: Watch out so i don't shoot you"]

diaChief = ["Einar: China white?", "Aina: Mr Einar you're under arrest", "Einar: You wont take me alive!"]

diaSchono = ["Einar: China white?", "Schono: Yeah, wanna fight me for it?", "Aina: 1v1 bro"]
