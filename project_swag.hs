import Control.Exception
import Prelude hiding(catch)
import Graphics.Gloss

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

menu = do
    putStrLn "Legend Of Einar And His Gucci Cap"
    putStrLn ""
    putStrLn "Play game = play | Quit Game = Press any key"
    answer <- getLine
    if answer == "play" then game else return ()


game = do
    putStrLn ""
    putStrLn "Einar vaktar alltid orten iklädd sin gucci keps och sin orten väst som säkert är skottsäker men det är oklart"
    putStrLn "En dag när Einar är ute och vaktar ser han en kran schono"
    putStrLn "Vad ska Einar göra:"
    putStrLn ""
    putStrLn "säga åt schonon att ge einar ny ladd. Press 1"
    putStrLn "börja hetsa schonon för att få han att få psykos. Press any other key than 1"
    answer <- getLine
    if answer == "1" then ladd
    else fightschono



ladd = do 
    putStrLn ""
    putStrLn "schonon kommer fram och ger Einar ladd" 
    putStrLn ""
    putStrLn "Einar: du är katten i trakten bror, du lyser upp hela natten <3"
    putStrLn "Schono: ehm okej..."
    putStrLn "Einar går hem för att ta sin ladd"

fightschono = do
    putStrLn ""
    putStrLn "schonon tappar det totalt och tar upp en becknaväska full med tegelsten."
    putStrLn "schonon slog ihjäl Einar, RIP Einar 2002-09-05"

